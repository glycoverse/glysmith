# ----- step_pca -----
test_that("step_pca generates plots and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_pca(loadings = TRUE))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("pca_samples" %in% names(res$tables))
  expect_true("pca_variables" %in% names(res$tables))
  expect_true("pca_eigenvalues" %in% names(res$tables))
  expect_true("pca_scores" %in% names(res$plots))
  expect_true("pca_loadings" %in% names(res$plots))
  expect_true("pca_screeplot" %in% names(res$plots))
})

test_that("step_pca works on sig_exp", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(100) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_dea_limma(), step_pca(on = "sig_exp", loadings = TRUE))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("pca_sig_samples" %in% names(res$tables))
  expect_true("pca_sig_variables" %in% names(res$tables))
  expect_true("pca_sig_eigenvalues" %in% names(res$tables))
  expect_true("pca_sig_scores" %in% names(res$plots))
  expect_true("pca_sig_loadings" %in% names(res$plots))
  expect_true("pca_sig_screeplot" %in% names(res$plots))
})

test_that("step_pca does not generate loadings plot when loadings = FALSE", {
  data(toy_experiment, package = "glyexp")
  ctx <- glysmith:::new_ctx(exp = toy_experiment, group_col = "group")
  step <- step_pca(on = "exp", loadings = FALSE)
  ctx_new <- step$run(ctx)
  expect_null(ctx_new$plots$pca_loadings)
  expect_true(!is.null(ctx_new$plots$pca_scores))
  expect_true(!is.null(ctx_new$plots$pca_screeplot))
})

test_that("step_pca generates loadings plot when loadings = TRUE", {
  data(toy_experiment, package = "glyexp")
  ctx <- glysmith:::new_ctx(exp = toy_experiment, group_col = "group")
  step <- step_pca(on = "exp", loadings = TRUE)
  ctx_new <- step$run(ctx)
  expect_true(!is.null(ctx_new$plots$pca_loadings))
  expect_true(!is.null(ctx_new$plots$pca_scores))
  expect_true(!is.null(ctx_new$plots$pca_screeplot))
})

# ----- step_tsne -----
test_that("step_tsne generates plots and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_tsne(perplexity = 2))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("tsne" %in% names(res$tables))
  expect_true("tsne" %in% names(res$plots))
})

# ----- step_umap -----
test_that("step_umap generates plots and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_umap(n_neighbors = 5))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("umap" %in% names(res$tables))
  expect_true("umap" %in% names(res$plots))
})

# ----- step_plsda -----
test_that("step_plsda generates plots and tables", {
  skip_if_not_installed("ropls")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "M")) |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_plsda())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("plsda_samples" %in% names(res$tables))
  expect_true("plsda_variables" %in% names(res$tables))
  expect_true("plsda_variance" %in% names(res$tables))
  expect_true("plsda_vip" %in% names(res$tables))
  expect_true("plsda_perm_test" %in% names(res$tables))
  expect_true("plsda_scores" %in% names(res$plots))
  expect_true("plsda_loadings" %in% names(res$plots))
  expect_true("plsda_variance" %in% names(res$plots))
  expect_true("plsda_vip" %in% names(res$plots))
})

test_that("step_plsda works on sig_exp", {
  skip_if_not_installed("ropls")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "M")) |>
      glyexp::slice_head_var(100) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_dea_limma(), step_plsda(on = "sig_exp"))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("plsda_sig_samples" %in% names(res$tables))
  expect_true("plsda_sig_variables" %in% names(res$tables))
  expect_true("plsda_sig_variance" %in% names(res$tables))
  expect_true("plsda_sig_vip" %in% names(res$tables))
  expect_true("plsda_sig_scores" %in% names(res$plots))
})

test_that("step_plsda respects ncomp parameter", {
  skip_if_not_installed("ropls")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "M")) |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_plsda(ncomp = 3))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("plsda_samples" %in% names(res$tables))
})

# ----- step_oplsda -----
test_that("step_oplsda generates plots and tables", {
  skip_if_not_installed("ropls")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "M")) |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  # Use ortho_i = 1 to ensure orthogonal components are created
  bp <- blueprint(step_oplsda(ortho_i = 1))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("oplsda_samples" %in% names(res$tables))
  expect_true("oplsda_variables" %in% names(res$tables))
  expect_true("oplsda_variance" %in% names(res$tables))
  expect_true("oplsda_vip" %in% names(res$tables))
  expect_true("oplsda_perm_test" %in% names(res$tables))
  expect_true("oplsda_scores" %in% names(res$plots))
  expect_true("oplsda_loadings" %in% names(res$plots))
  expect_true("oplsda_variance" %in% names(res$plots))
  expect_true("oplsda_vip" %in% names(res$plots))
})

test_that("step_oplsda skips multi-group experiment", {
  skip_if_not_installed("ropls")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_oplsda())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_false("oplsda_samples" %in% names(res$tables))
})

test_that("step_oplsda works with custom parameters", {
  skip_if_not_installed("ropls")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "M")) |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_oplsda(pred_i = 1, ortho_i = 2))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("oplsda_samples" %in% names(res$tables))
})
