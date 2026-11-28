# ----- step_pca -----
make_pca_exp <- function() {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(
      abundance = matrix(
        seq_len(24),
        nrow = 4,
        dimnames = list(paste0("V", 1:4), paste0("S", 1:6))
      )
    ),
    colData = S4Vectors::DataFrame(
      group = factor(rep(c("A", "B"), each = 3)),
      row.names = paste0("S", 1:6)
    )
  )
}

test_that("step_pca generates plots and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_pca(loadings = TRUE))
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1__pca_samples" %in% names(res$tables))
  expect_true("node_1__pca_variables" %in% names(res$tables))
  expect_true("node_1__pca_eigenvalues" %in% names(res$tables))
  expect_true("node_1__pca_scores" %in% names(res$plots))
  expect_true("node_1__pca_loadings" %in% names(res$plots))
  expect_true("node_1__pca_screeplot" %in% names(res$plots))
})

test_that("step_pca works on sig_exp", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(100) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma() %=>%
      step_filter_sig_vars() %=>%
      step_pca(loadings = TRUE)
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__pca_samples" %in% names(res$tables))
  expect_true("node_3__pca_variables" %in% names(res$tables))
  expect_true("node_3__pca_eigenvalues" %in% names(res$tables))
  expect_true("node_3__pca_scores" %in% names(res$plots))
  expect_true("node_3__pca_loadings" %in% names(res$plots))
  expect_true("node_3__pca_screeplot" %in% names(res$plots))
})

test_that("step_pca does not generate loadings plot when loadings = FALSE", {
  ctx <- glysmith:::new_ctx(exp = make_pca_exp(), group_col = "group")
  step <- step_pca(loadings = FALSE)
  ctx_new <- step$run(ctx)
  expect_null(ctx_new$plots$pca_loadings)
  expect_true(!is.null(ctx_new$plots$pca_scores))
  expect_true(!is.null(ctx_new$plots$pca_screeplot))
})

test_that("step_pca generates loadings plot when loadings = TRUE", {
  ctx <- glysmith:::new_ctx(exp = make_pca_exp(), group_col = "group")
  step <- step_pca(loadings = TRUE)
  ctx_new <- step$run(ctx)
  expect_true(!is.null(ctx_new$plots$pca_loadings))
  expect_true(!is.null(ctx_new$plots$pca_scores))
  expect_true(!is.null(ctx_new$plots$pca_screeplot))
})

test_that("step_pca does not generate screeplot when screeplot = FALSE", {
  ctx <- glysmith:::new_ctx(exp = make_pca_exp(), group_col = "group")
  step <- step_pca(screeplot = FALSE)
  ctx_new <- step$run(ctx)
  expect_null(ctx_new$plots$pca_screeplot)
  expect_true(!is.null(ctx_new$plots$pca_scores))
  expect_null(ctx_new$plots$pca_loadings)
})

test_that("step_pca generates screeplot when screeplot = TRUE", {
  ctx <- glysmith:::new_ctx(exp = make_pca_exp(), group_col = "group")
  step <- step_pca(screeplot = TRUE)
  ctx_new <- step$run(ctx)
  expect_true(!is.null(ctx_new$plots$pca_screeplot))
  expect_true(!is.null(ctx_new$plots$pca_scores))
  expect_null(ctx_new$plots$pca_loadings)
})

# ----- step_tsne -----
test_that("step_tsne generates plots and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_tsne(perplexity = 2))
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1__tsne" %in% names(res$tables))
  expect_true("node_1__tsne" %in% names(res$plots))
})

# ----- step_umap -----
test_that("step_umap generates plots and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(50) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_umap(n_neighbors = 5))
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1__umap" %in% names(res$tables))
  expect_true("node_1__umap" %in% names(res$plots))
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
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1__plsda_samples" %in% names(res$tables))
  expect_true("node_1__plsda_variables" %in% names(res$tables))
  expect_true("node_1__plsda_variance" %in% names(res$tables))
  expect_true("node_1__plsda_vip" %in% names(res$tables))
  expect_true("node_1__plsda_perm_test" %in% names(res$tables))
  expect_true("node_1__plsda_scores" %in% names(res$plots))
  expect_true("node_1__plsda_loadings" %in% names(res$plots))
  expect_true("node_1__plsda_variance" %in% names(res$plots))
  expect_true("node_1__plsda_vip" %in% names(res$plots))
})

test_that("step_plsda works on sig_exp", {
  skip_if_not_installed("ropls")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "M")) |>
      glyexp::slice_head_var(100) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma() %=>% step_filter_sig_vars() %=>% step_plsda()
  )
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_3__plsda_samples" %in% names(res$tables))
  expect_true("node_3__plsda_variables" %in% names(res$tables))
  expect_true("node_3__plsda_variance" %in% names(res$tables))
  expect_true("node_3__plsda_vip" %in% names(res$tables))
  expect_true("node_3__plsda_scores" %in% names(res$plots))
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
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1__plsda_samples" %in% names(res$tables))
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
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1__oplsda_samples" %in% names(res$tables))
  expect_true("node_1__oplsda_variables" %in% names(res$tables))
  expect_true("node_1__oplsda_variance" %in% names(res$tables))
  expect_true("node_1__oplsda_vip" %in% names(res$tables))
  expect_true("node_1__oplsda_perm_test" %in% names(res$tables))
  expect_true("node_1__oplsda_scores" %in% names(res$plots))
  expect_true("node_1__oplsda_loadings" %in% names(res$plots))
  expect_true("node_1__oplsda_variance" %in% names(res$plots))
  expect_true("node_1__oplsda_vip" %in% names(res$plots))
})

test_that("step_oplsda skips multi-group experiment", {
  skip_if_not_installed("ropls")
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_oplsda())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_false("node_1__oplsda_samples" %in% names(res$tables))
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
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1__oplsda_samples" %in% names(res$tables))
})


test_that("OPLS-DA accepts two observed groups in a custom character column", {
  exp <- make_pca_exp()
  SummarizedExperiment::colData(exp)$cohort <- rep(c("A", "B"), each = 3)
  ctx <- glysmith:::new_ctx(exp, group_col = "cohort")

  result <- glysmith:::.condition_oplsda(ctx)

  expect_true(result$check)
  expect_null(result$reason)
})

test_that("OPLS-DA group checks ignore unused factor levels", {
  exp <- make_pca_exp()
  SummarizedExperiment::colData(exp)$group <- factor(
    rep(c("A", "B"), each = 3),
    levels = c("A", "B", "unused")
  )
  ctx <- glysmith:::new_ctx(exp, group_col = "group")

  expect_true(glysmith:::.condition_oplsda(ctx)$check)

  ctx$data$exp <- exp[, 1:3]
  result <- glysmith:::.condition_oplsda(ctx)
  expect_false(result$check)
  expect_match(result$reason, "found 1 groups")
})
