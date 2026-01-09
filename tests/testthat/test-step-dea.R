# ----- step_dea_limma -----
test_that("step_dea_limma generates results and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_dea_limma())
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("dea_res" %in% names(res$data))
  expect_true("sig_exp" %in% names(res$data))
  expect_true("dea" %in% names(res$tables))
})

test_that("step_dea_limma works on trait_exp", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_derive_traits(),
    step_dea_limma(on = "trait_exp")
  )
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("dta_res" %in% names(res$data))
  expect_true("sig_trait_exp" %in% names(res$data))
  expect_true("dta" %in% names(res$tables))
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
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("dea_res" %in% names(res$data))
  expect_true("dea" %in% names(res$tables))
})

test_that("step_dea_ttest skips multi-group experiment", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_dea_ttest())
  suppressMessages(expect_message(res <- forge_analysis(exp, bp), "failed"))
  expect_null(res$data$dea_res)
  expect_null(res$tables$dea)
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
  bp <- blueprint(step_dea_wilcox())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("dea_res" %in% names(res$data))
  expect_true("sig_exp" %in% names(res$data))
  expect_true("dea" %in% names(res$tables))
})

# ----- step_dea_anova -----
test_that("step_dea_anova generates results for multi-group", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  # Ensure we have multi-group
  expect_gt(length(unique(exp$sample_info$group)), 2)

  bp <- blueprint(step_dea_anova())
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("dea_res" %in% names(res$data))
  expect_true("dea_main_test" %in% names(res$tables))
  expect_true("dea_post_hoc_test" %in% names(res$tables))
})

# ----- step_dea_kruskal -----
test_that("step_dea_kruskal generates results for multi-group", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  expect_gt(length(unique(exp$sample_info$group)), 2)

  bp <- blueprint(step_dea_kruskal())
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("dea_res" %in% names(res$data))
  expect_true("dea_main_test" %in% names(res$tables))
  expect_true("dea_post_hoc_test" %in% names(res$tables))
})
