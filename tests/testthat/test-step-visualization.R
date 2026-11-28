# ----- step_heatmap -----
test_that("step_heatmap generates plot", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_heatmap())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1__heatmap" %in% names(res$plots))
})

test_that("step_heatmap works on sig_exp", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  # We need DEA to get sig_exp
  bp <- blueprint(
    step_dea_limma() %=>%
      step_filter_sig_vars() %=>%
      step_heatmap()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__heatmap" %in% names(res$plots))
})

# ----- step_plot_glycosite_profile -----
test_that("step_plot_glycosite_profile counts distinct glycans and sites", {
  skip_if_not_installed("glyvis", "0.9.0")
  exp <- glyexp::real_experiment[1:5, ]
  info <- SummarizedExperiment::rowData(exp)
  info$protein <- c("A", "A", "A", "A", "B")
  info$protein_site <- c(10L, 10L, 10L, 20L, 10L)
  info$glycan_id <- c("g1", "g1", "g2", "g1", "g2")
  SummarizedExperiment::rowData(exp) <- info
  bp <- blueprint(step_plot_glycosite_profile(glycan_col = "glycan_id"))

  result <- forge_analysis_se(exp, bp)

  expect_named(
    result$plots,
    c("node_1__glycans_per_site", "node_1__sites_per_protein")
  )
  glycans <- cast_plot(result, "node_1__glycans_per_site")
  sites <- cast_plot(result, "node_1__sites_per_protein")
  expect_s3_class(glycans, "ggplot")
  expect_s3_class(sites, "ggplot")
  expect_equal(glycans$data$count, c(2, 1, rep(0, 6)))
  expect_equal(sites$data$count, c(1, 1, rep(0, 6)))
})

test_that("step_plot_glycosite_profile skips glycomics", {
  bp <- blueprint(step_plot_glycosite_profile())
  result <- forge_analysis_se(glyexp::real_experiment2, bp)
  expect_length(result$plots, 0)
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
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_false("node_1__logo" %in% names(res$plots))
})

# ----- step_sig_boxplot -----
test_that("step_sig_boxplot generates plot", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma() %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
})

test_that("step_sig_boxplot with n_top limit", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma() %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot(n_top = 10)
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
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
    step_dea_limma() %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
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
    step_dea_ttest(filter_p_adj_cutoff = 1) %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
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
    step_dea_wilcox(filter_p_adj_cutoff = 1) %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
})

test_that("step_sig_boxplot works with step_dea_anova", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_anova() %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
})

test_that("step_sig_boxplot ranks top variables after step_dea_anova", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_anova(filter_p_adj_cutoff = 1) %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true(nrow(res$data$node_2) > 25)
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
})

test_that("step_sig_boxplot works with step_dea_kruskal", {
  skip_if_not_installed("FSA")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_kruskal(filter_p_adj_cutoff = 1) %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
})

test_that("step_sig_boxplot ranks top variables after step_dea_kruskal", {
  skip_if_not_installed("FSA")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_kruskal(filter_p_adj_cutoff = 1) %=>%
      step_filter_sig_vars() %=>%
      step_sig_boxplot()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true(nrow(res$data$node_2) > 25)
  expect_true("node_3__sig_boxplot" %in% names(res$plots))
})
