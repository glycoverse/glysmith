# ----- step_dea_limma -----
test_that("step_dea_limma generates results and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_dea_limma(), step_filter_sig_vars())
  suppressMessages(res <- forge_analysis_se(exp, bp))

  expect_true("node_1" %in% names(res$data))
  expect_true("node_2" %in% names(res$data))
  expect_true("node_1__dea" %in% names(res$tables))
})

test_that("step_dea_limma works on trait_exp", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_derive_traits(),
    step_dea_limma(),
    step_filter_sig_vars()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))

  expect_true("node_2" %in% names(res$data))
  expect_true("node_3" %in% names(res$data))
  expect_true("node_2__dea" %in% names(res$tables))
})

# ----- step_dea_ttest -----
test_that("step_dea_ttest generates results", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_dea_ttest())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1" %in% names(res$data))
  expect_true("node_1__dea" %in% names(res$tables))
})

test_that("step_dea_ttest skips multi-group experiment", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_dea_ttest())
  suppressMessages(expect_message(res <- forge_analysis_se(exp, bp), "failed"))
  expect_null(res$data$node_1)
  expect_null(res$tables$node_1__dea)
})

# ----- step_dea_wilcox -----
test_that("step_dea_wilcox generates results", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_dea_wilcox(), step_filter_sig_vars())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1" %in% names(res$data))
  expect_true("node_2" %in% names(res$data))
  expect_true("node_1__dea" %in% names(res$tables))
})

# ----- step_dea_anova -----
test_that("step_dea_anova generates results for multi-group", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  # Ensure we have multi-group
  expect_gt(
    length(unique(SummarizedExperiment::colData(as_test_glyco_se(exp))$group)),
    2
  )

  bp <- blueprint(step_dea_anova())
  suppressMessages(res <- forge_analysis_se(exp, bp))

  expect_true("node_1" %in% names(res$data))
  expect_true("node_1__dea_main_test" %in% names(res$tables))
  expect_true("node_1__dea_post_hoc_test" %in% names(res$tables))
})

# ----- step_dea_kruskal -----
test_that("step_dea_kruskal generates results for multi-group", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  expect_gt(
    length(unique(SummarizedExperiment::colData(as_test_glyco_se(exp))$group)),
    2
  )

  bp <- blueprint(step_dea_kruskal())
  suppressMessages(res <- forge_analysis_se(exp, bp))

  expect_true("node_1" %in% names(res$data))
  expect_true("node_1__dea_main_test" %in% names(res$tables))
  expect_true("node_1__dea_post_hoc_test" %in% names(res$tables))
})
