connected <- function(...) blueprint(..., .mode = "connected")

test_that("construction checks every link, including independent sub-blueprints", {
  selected <- connected(step_filter_sig_vars(), step_heatmap())
  expect_error(connected(step_derive_traits(), selected), "requires")
  expect_error(connected(step_pca(), step_heatmap()), "terminal")
  expect_error(
    connected(fork(step_pca(), step_heatmap()), step_heatmap()),
    "fork"
  )
  expect_error(connected(step_dea_anova(), step_volcano()), "requires")
  expect_error(connected(step_dea_ttest(), step_plot_main_test()), "requires")
  expect_error(connected(fork(step_pca(), step_filter_sig_vars())), "requires")
  expect_error(connected(step_heatmap(on = "trait_exp")), "legacy on")
  bp <- connected(
    step_derive_traits(),
    fork(
      scores = step_pca(),
      tests = connected(step_dea_anova(), fork(step_plot_main_test(), selected))
    )
  )
  expect_output(print(bp), "node_3 -> node_4.*step_plot_main_test.*tests")
  expect_output(print(bp), "node_3 -> node_5.*step_filter_sig_vars")
  path <- tempfile()
  write_blueprint(bp, path)
  expect_equal(
    capture.output(print(read_blueprint(path))),
    capture.output(print(bp))
  )
})

test_that("real traits feed PCA and one omnibus fit with independent consumers", {
  bp <- connected(
    step_preprocess(),
    step_derive_traits(),
    fork(
      step_pca(),
      connected(
        step_dea_anova(),
        fork(
          step_plot_main_test(),
          connected(step_filter_sig_vars(), step_heatmap())
        )
      )
    )
  )
  skip_if_glysmith_deps_not_installed(bp)
  x <- suppressMessages(forge_analysis(glyexp::real_experiment2, bp))
  expect_identical(
    unname(vapply(x$meta$nodes, "[[", "", "status")),
    rep("success", 7)
  )
  traits <- cast_data(x, "node_2")
  fit <- cast_data(x, "node_4")
  expect_identical(cast_exp(x), traits)
  expect_equal(
    cast_data(x, "node_6"),
    glystats::filter_sig_vars(traits, fit, fc_cutoff = NULL)
  )
  expect_s3_class(cast_plot(x, "node_5__main_test"), "ggplot")
  expect_s3_class(cast_plot(x, "node_7__heatmap"), "ggplot")
  expect_equal(
    cast_table(x, "node_4__dea_main_test"),
    glystats::get_tidy_result(fit, "main_test")
  )
  expect_match(x$meta$reports$node_6, "Retained")
  expect_false(any(grepl("Report generation failed", unlist(x$meta$reports))))
})

# Keep the registered GlycomicSE annotations and replace its entire assay
# with deterministic group effects.
connected_fixture <- function() {
  exp <- glyexp::real_experiment2[1:4, 1:12]
  group <- factor(rep(c("A", "B", "C"), each = 4L))
  SummarizedExperiment::colData(exp)$group <- group
  vals <- outer(c(10, 15, 20, 25), rep(c(0.9, 1.1, 1.2, 0.8), 3L))
  vals[2, group == "B"] <- 4 * vals[2, group == "B"]
  vals[3, group == "C"] <- 8 * vals[3, group == "C"]
  dimnames(vals) <- dimnames(exp)
  SummarizedExperiment::assay(exp) <- vals
  exp
}

test_that("a shared transformation runs once and thresholds reuse the exact fit", {
  prep_calls <- fit_calls <- 0L
  observed <- list()
  original_fit <- .fit_dea
  local_mocked_bindings(.fit_dea = function(ctx, exp, ...) {
    fit_calls <<- fit_calls + 1L
    observed[[fit_calls]] <<- exp
    original_fit(ctx, exp, ...)
  })
  prep <- step_zero_to_na()
  run <- prep$run
  prep$run <- function(ctx) {
    prep_calls <<- prep_calls + 1L
    run(ctx)
  }
  tests <- connected(
    step_dea_anova(),
    fork(step_filter_sig_vars(0), step_filter_sig_vars(0.05))
  )
  bp <- connected(
    prep,
    fork(
      full = tests,
      subset = connected(step_subset_groups(c("A", "B")), tests)
    )
  )
  skip_if_glysmith_deps_not_installed(bp)
  x <- suppressMessages(forge_analysis(connected_fixture(), bp))
  expect_identical(prep_calls, 1L)
  expect_identical(fit_calls, 2L)
  expect_identical(observed, list(x$data$node_1, x$data$node_5))
  expect_identical(x$data$root, x$data$node_1__raw_exp)
  expect_identical(ncol(observed[[1]]), 12L)
  expect_identical(ncol(observed[[2]]), 8L)
  expect_true(all(vapply(x$meta$nodes, "[[", "", "status") == "success"))
  expect_identical(nrow(x$data$node_3), 0L)
  expect_identical(nrow(x$data$node_7), 0L)
  expect_equal(x$data$node_4, observed[[1]][2:3, ])
  expect_equal(x$data$node_8, observed[[2]][2, ])
  expect_identical(colnames(x$data$node_8), colnames(observed[[2]]))
})

test_that("skips and failures block only descendants and preserve their cause", {
  tests <- connected(step_dea_anova(), step_filter_sig_vars(0), step_heatmap())
  bp <- connected(
    br("ok", tests),
    br("skipped", step_subset_groups(), tests),
    br("failed", step_subset_groups("unknown"), tests),
    step_pca()
  )
  skip_if_glysmith_deps_not_installed(bp)
  x <- suppressMessages(forge_analysis(connected_fixture(), bp))
  state <- unname(vapply(x$meta$nodes, "[[", "", "status"))
  expect_identical(
    state,
    c(
      "success",
      "success",
      "skipped",
      "skipped",
      rep("blocked", 3),
      "failed",
      rep("blocked", 3),
      "success"
    )
  )
  expect_identical(
    x$meta$nodes$node_3$reason,
    "input experiment has zero variables"
  )
  expect_identical(x$meta$nodes$node_7$reason, x$meta$nodes$node_4$reason)
  expect_match(x$meta$nodes$node_11$reason, "unknown")
  expect_null(x$data$node_6)
  expect_null(x$data$node_10)
  expect_identical(x$meta$nodes$node_12$parent, "root")
})

test_that("volcano eligibility follows the fit on its own branch", {
  tests <- connected(step_dea_limma(), step_volcano())
  bp <- connected(
    step_subset_groups(c("A", "B")),
    fork(
      connected(step_coda("clr", gamma = 0), tests),
      tests
    )
  )
  skip_if_glysmith_deps_not_installed(bp)
  x <- suppressMessages(forge_analysis(connected_fixture(), bp))
  expect_identical(x$meta$nodes$node_4$status, "skipped")
  expect_match(x$meta$nodes$node_4$reason, "logx")
  expect_identical(x$meta$nodes$node_6$status, "success")
})

test_that("each catalogued step declares its connected input", {
  expect_true(all(vapply(
    all_steps(),
    function(s) length(s$connected$accepts) > 0L,
    logical(1)
  )))
})

test_that("a failed fit never supplies an older fit to its filters", {
  bp <- connected(fork(
    connected(step_dea_anova(), step_filter_sig_vars()),
    connected(step_dea_ttest(), step_filter_sig_vars())
  ))
  skip_if_glysmith_deps_not_installed(bp)
  x <- suppressMessages(forge_analysis(connected_fixture(), bp))
  expect_identical(x$meta$nodes$node_2$status, "success")
  expect_identical(x$meta$nodes$node_3$status, "failed")
  expect_identical(x$meta$nodes$node_4$status, "blocked")
  expect_identical(x$meta$nodes$node_4$reason, x$meta$nodes$node_3$reason)
  expect_null(x$data$node_4)
})

test_that("mode-specific arguments guide callers to the correct input", {
  expect_error(
    connected(step_dea_anova(filter_p_adj_cutoff = 0.1)),
    "step_filter_sig_vars"
  )
  expect_error(
    blueprint(step_dea_anova(), br("select", step_filter_sig_vars())),
    "connected"
  )
})
