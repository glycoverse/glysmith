# ----- step_pca -----
test_that("step_pca generates plots and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_pca())
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
  bp <- blueprint(step_dea_limma(), step_pca(on = "sig_exp"))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("pca_sig_samples" %in% names(res$tables))
  expect_true("pca_sig_variables" %in% names(res$tables))
  expect_true("pca_sig_eigenvalues" %in% names(res$tables))
  expect_true("pca_sig_scores" %in% names(res$plots))
  expect_true("pca_sig_loadings" %in% names(res$plots))
  expect_true("pca_sig_screeplot" %in% names(res$plots))
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
