# ----- step_heatmap -----
test_that("step_heatmap generates plot", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_heatmap())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("heatmap" %in% names(res$plots))
})

test_that("step_heatmap works on sig_exp", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  # We need DEA to get sig_exp
  bp <- blueprint(
    step_dea_limma(),
    step_heatmap(on = "sig_exp")
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("heatmap_sig" %in% names(res$plots))
})

# ----- step_logo -----
test_that("step_logo is skipped for glycomics experiments", {
  skip_if_not_installed("ggseqlogo")
  suppressMessages(
    exp <- glyexp::real_experiment2 |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_logo())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_false("logo" %in% names(res$plots))
})

# ----- step_sig_boxplot -----
test_that("step_sig_boxplot generates plot", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma(),
    step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("sig_boxplot_sig" %in% names(res$plots))
})

test_that("step_sig_boxplot with n_top limit", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma(),
    step_sig_boxplot(n_top = 10)
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("sig_boxplot_sig" %in% names(res$plots))
})

test_that("step_sig_boxplot validates n_top range", {
  expect_error(step_sig_boxplot(n_top = 0), "not >= 1")
  expect_error(step_sig_boxplot(n_top = 26), "not <= 25")
})

# ----- step_sig_boxplot with all DEA methods -----
test_that("step_sig_boxplot works with step_dea_limma", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma(),
    step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("sig_boxplot_sig" %in% names(res$plots))
})

test_that("step_sig_boxplot works with step_dea_ttest", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_ttest(),
    step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("sig_boxplot_sig" %in% names(res$plots))
})

test_that("step_sig_boxplot works with step_dea_wilcox", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_wilcox(),
    step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("sig_boxplot_sig" %in% names(res$plots))
})

test_that("step_sig_boxplot works with step_dea_anova", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_anova(),
    step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("sig_boxplot_sig" %in% names(res$plots))
})

test_that("step_sig_boxplot works with step_dea_kruskal", {
  skip_if_not_installed("FSA")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_kruskal(),
    step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("sig_boxplot_sig" %in% names(res$plots))
})
