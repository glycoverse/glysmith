test_that("construction checks every link, including independent sub-blueprints", {
  selected <- step_filter_sig_vars() %=>% step_heatmap()
  expect_identical(
    collect_blueprint_packages(blueprint(step_dea_anova())),
    "glystats"
  )
  expect_error(blueprint(step_derive_traits() %=>% selected), "requires")
  expect_error(blueprint(step_pca() %=>% step_heatmap()), "terminal")
  expect_error(
    blueprint(fork(step_pca(), step_heatmap()) %=>% step_heatmap()),
    "fork"
  )
  expect_error(blueprint(step_dea_anova() %=>% step_volcano()), "requires")
  expect_error(blueprint(fork(step_pca(), step_filter_sig_vars())), "requires")
  expect_error(blueprint(step_heatmap(on = "trait_exp")), "argument")
  bp <- blueprint(
    br("overview", step_ident_overview()) %=>%
      step_derive_traits() %=>%
      fork(
        tests = blueprint(
          step_dea_anova() %=>%
            fork(step_filter_sig_vars(0.01), selected)
        ),
        scores = step_pca()
      )
  )
  expect_snapshot(print(bp))
  path <- tempfile()
  write_blueprint(bp, path)
  expect_equal(
    capture.output(print(read_blueprint(path))),
    capture.output(print(bp))
  )
})

test_that("real traits feed PCA and one omnibus fit with independent consumers", {
  bp <- blueprint(
    step_preprocess() %=>%
      step_derive_traits() %=>%
      fork(
        step_pca(),
        blueprint(
          step_dea_anova() %=>%
            fork(
              step_filter_sig_vars(0.01),
              step_filter_sig_vars() %=>% step_heatmap()
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
  expect_equal(
    cast_data(x, "node_5"),
    glystats::filter_sig_vars(
      traits,
      fit,
      p_adj_cutoff = 0.01,
      fc_cutoff = NULL
    )
  )
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
  tests <- blueprint(
    step_dea_anova() %=>%
      fork(step_filter_sig_vars(0), step_filter_sig_vars(0.05))
  )
  bp <- blueprint(
    prep %=>%
      fork(
        full = tests,
        subset = blueprint(step_subset_groups(c("A", "B")) %=>% tests)
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
  tests <- blueprint(
    step_dea_anova() %=>% step_filter_sig_vars(0) %=>% step_heatmap()
  )
  bp <- blueprint(
    br("ok", tests) %=>%
      br("skipped", step_subset_groups() %=>% tests) %=>%
      br("failed", step_subset_groups("unknown") %=>% tests) %=>%
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
  tests <- blueprint(step_dea_limma() %=>% step_volcano())
  bp <- blueprint(
    step_subset_groups(c("A", "B")) %=>%
      fork(
        blueprint(step_coda("clr", gamma = 0) %=>% tests),
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
  bp <- blueprint(fork(
    blueprint(step_dea_anova() %=>% step_filter_sig_vars()),
    blueprint(step_dea_ttest() %=>% step_filter_sig_vars())
  ))
  skip_if_glysmith_deps_not_installed(bp)
  x <- suppressMessages(forge_analysis(connected_fixture(), bp))
  expect_identical(x$meta$nodes$node_2$status, "success")
  expect_identical(x$meta$nodes$node_3$status, "failed")
  expect_identical(x$meta$nodes$node_4$status, "blocked")
  expect_identical(x$meta$nodes$node_4$reason, x$meta$nodes$node_3$reason)
  expect_null(x$data$node_4)
})

test_that("filter defaults from an omnibus fit can be overridden per consumer", {
  bp <- blueprint(
    step_dea_anova(filter_on = "main_test", filter_p_adj_cutoff = 0) %=>%
      fork(step_filter_sig_vars(), step_filter_sig_vars(0.05))
  )
  skip_if_glysmith_deps_not_installed(bp)
  x <- forge_analysis(connected_fixture(), bp)
  expect_identical(nrow(x$data$node_2), 0L)
  expect_identical(nrow(x$data$node_3), 2L)
})

test_that("a filter can override its fit's post-hoc comparison with the main test", {
  bp <- blueprint(
    step_dea_anova(
      filter_on = "post_hoc_test",
      filter_comparison = "A_vs_B"
    ) %=>%
      fork(
        step_filter_sig_vars(),
        step_filter_sig_vars(on = "main_test", comparison = NULL)
      )
  )
  skip_if_glysmith_deps_not_installed(bp)
  x <- forge_analysis(connected_fixture(), bp)
  expect_true(all(vapply(x$meta$nodes, "[[", "", "status") == "success"))
  expect_equal(
    x$data$node_2,
    glystats::filter_sig_vars(
      x$data$root,
      x$data$node_1,
      on = "post_hoc_test",
      comparison = "A_vs_B"
    )
  )
  expect_equal(
    x$data$node_3,
    glystats::filter_sig_vars(x$data$root, x$data$node_1)
  )
})

test_that("failure in a root route leaves the original experiment and its siblings available", {
  bad <- step_dea_ttest() %=>% step_filter_sig_vars()
  bp <- blueprint(
    bad = bad,
    good = step_dea_anova() %=>% step_filter_sig_vars(0.05)
  )
  skip_if_glysmith_deps_not_installed(bp)
  x <- suppressMessages(forge_analysis(connected_fixture(), bp))
  expect_identical(
    unname(vapply(x$meta$nodes, "[[", "", "status")),
    c("failed", "blocked", "success", "success")
  )
  expect_identical(x$meta$nodes$node_2$reason, x$meta$nodes$node_1$reason)
  expect_identical(cast_exp(x), x$data$root)
  expect_identical(x$data$node_4, x$data$root[2:3, ])
})

test_that("single, independent and shared preprocessing bind both consumers to each actual fit", {
  limma <- blueprint(
    step_dea_limma() %=>%
      fork(
        step_volcano(),
        step_filter_sig_vars() %=>% step_heatmap()
      )
  )
  ttest <- blueprint(
    step_dea_ttest() %=>%
      fork(
        step_volcano(),
        step_filter_sig_vars() %=>% step_heatmap()
      )
  )
  single <- blueprint(
    step_preprocess() %=>%
      step_dea_limma() %=>%
      fork(
        step_volcano(),
        step_filter_sig_vars() %=>% step_heatmap()
      )
  )
  separate <- blueprint(
    step_preprocess() %=>% limma,
    step_preprocess() %=>% ttest,
  )
  shared <- blueprint(step_preprocess() %=>% fork(limma, ttest))
  skip_if_glysmith_deps_not_installed(shared)
  input <- glyexp::real_experiment2
  input <- input[, input$group %in% c("H", "C")]

  # Observe the real backends, including the exact objects sent to the filter.
  preprocess <- .run_preprocess
  fit <- .fit_dea
  filter <- glystats::filter_sig_vars
  local_mocked_bindings(
    .run_preprocess = function(ctx, ...) {
      preps <<- preps + 1L
      preprocess(ctx, ...)
    },
    .fit_dea = function(ctx, exp, method, ...) {
      result <- fit(ctx, exp, method, ...)
      fits[[method]] <<- list(exp = exp, fit = result$data$dea_res)
      methods_run <<- c(methods_run, method)
      result
    }
  )
  local_mocked_bindings(
    filter_sig_vars = function(exp, res, ...) {
      filters[[length(filters) + 1L]] <<- list(exp = exp, fit = res)
      filter(exp, res, ...)
    },
    .package = "glystats"
  )
  for (plan in c("single", "separate", "shared")) {
    bp <- switch(plan, single = single, separate = separate, shared = shared)
    preps <- 0L
    methods_run <- character()
    fits <- filters <- list()
    x <- suppressMessages(forge_analysis(input, bp))
    expect_true(all(vapply(x$meta$nodes, "[[", "", "status") == "success"))
    expect_identical(preps, if (plan == "separate") 2L else 1L)
    expect_identical(
      methods_run,
      if (plan == "single") "limma" else c("limma", "ttest")
    )
    expect_identical(filters, unname(fits))
    expect_identical(fits$limma$exp, x$data$node_1)
    expect_identical(x$meta$nodes$node_3$parent, "node_2")
    expect_identical(x$meta$nodes$node_4$parent, "node_2")
    expect_length(cast_plot(x), 2L * length(methods_run))
    expect_identical(
      cast_exp(x),
      if (plan == "separate") x$data$root else x$data$node_1
    )
    if (plan != "single") {
      ttest_input <- if (plan == "separate") "node_6" else "node_1"
      expect_identical(fits$ttest$exp, cast_data(x, ttest_input))
    }
  }
})
