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
