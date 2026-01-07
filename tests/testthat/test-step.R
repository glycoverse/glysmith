# ----- step_preprocess -----
test_that("step_preprocess overwrites exp and writes raw_exp", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  # We use sum to check if the expression matrix is changed
  old_sum <- sum(exp$expr_mat, na.rm = TRUE)
  bp <- blueprint(step_preprocess())
  suppressMessages(res <- forge_analysis(exp, bp))
  new_exp <- res$data$exp
  raw_exp <- res$data$raw_exp
  expect_equal(sum(raw_exp$expr_mat, na.rm = TRUE), old_sum)
  expect_false(sum(new_exp$expr_mat, na.rm = TRUE) == old_sum)
})

test_that("step_preprocess respects pre_qc and post_qc flags", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  bp <- blueprint(step_preprocess(pre_qc = TRUE, post_qc = TRUE))
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("qc_pre_missing_heatmap" %in% names(res$plots))
  expect_true("qc_missing_heatmap" %in% names(res$plots))

  bp_no_qc <- blueprint(step_preprocess(pre_qc = FALSE, post_qc = FALSE))
  suppressMessages(res_no_qc <- forge_analysis(exp, bp_no_qc))
  expect_false("qc_pre_missing_heatmap" %in% names(res_no_qc$plots))
  expect_false("qc_missing_heatmap" %in% names(res_no_qc$plots))
})

# ----- step_ident_overview -----
test_that("step_ident_overview generates summary", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  bp <- blueprint(step_ident_overview())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("summary" %in% names(res$tables))
})

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

# ----- step_derive_traits -----
test_that("step_derive_traits generates trait_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_derive_traits())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("trait_exp" %in% names(res$data))
  expect_true("derived_traits" %in% names(res$tables))
})

# ----- step_quantify_motifs -----
test_that("step_quantify_motifs generates motif_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_quantify_motifs())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("motif_exp" %in% names(res$data))
  expect_true("quantified_motifs" %in% names(res$tables))
})

# ----- step_adjust_protein -----
make_protein_expr <- function(exp) {
  protein_tbl <- exp$var_info |>
    dplyr::select(dplyr::all_of(c("variable", "protein")))

  expr_tbl <- tibble::as_tibble(exp$expr_mat, rownames = "variable") |>
    dplyr::left_join(protein_tbl, by = "variable") |>
    dplyr::group_by(.data$protein) |>
    dplyr::summarise(
      dplyr::across(-dplyr::all_of("variable"), ~ stats::median(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  list(
    tbl = expr_tbl,
    mat = expr_tbl |>
      tibble::column_to_rownames("protein") |>
      as.matrix()
  )
}

test_that("step_adjust_protein adjusts exp from csv/tsv/rds", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(50)

  pro_expr <- make_protein_expr(exp)
  csv_path <- tempfile(fileext = ".csv")
  tsv_path <- tempfile(fileext = ".tsv")
  rds_path <- tempfile(fileext = ".rds")

  readr::write_csv(pro_expr$tbl, csv_path)
  readr::write_tsv(pro_expr$tbl, tsv_path)
  readr::write_rds(pro_expr$mat, rds_path)

  run_adjust <- function(path) {
    bp <- blueprint(step_adjust_protein(path))
    suppressMessages(forge_analysis(exp, bp))
  }

  purrr::walk(c(csv_path, tsv_path, rds_path), function(path) {
    res <- run_adjust(path)
    expect_true("unadj_exp" %in% names(res$data))
    expect_true("exp" %in% names(res$data))
    expect_false(
      isTRUE(all.equal(
        sum(res$data$exp$expr_mat, na.rm = TRUE),
        sum(res$data$unadj_exp$expr_mat, na.rm = TRUE)
      ))
    )
  })
})

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

# ----- step_sig_enrich -----
run_sig_enrich_step <- function(step_fun, kind) {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("org.Hs.eg.db")

  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )

  ctx <- new_ctx(exp, "group")
  ctx <- ctx_add_data(ctx, "sig_exp", exp)

  mock_tbl <- tibble::tibble(
    description = c("term_a", "term_b"),
    p_adj = c(0.01, 0.2)
  )
  local_mocked_bindings(
    gly_enrich_go = function(...) structure(list(), class = "mock_enrich"),
    gly_enrich_kegg = function(...) structure(list(), class = "mock_enrich"),
    gly_enrich_reactome = function(...) structure(list(), class = "mock_enrich"),
    get_tidy_result = function(...) mock_tbl,
    .package = "glystats"
  )
  local_mocked_bindings(
    plot_enrich = function(...) ggplot2::ggplot(),
    .package = "glyvis"
  )

  step_obj <- step_fun()
  expect_true(step_obj$condition(ctx)$check)
  ctx <- step_obj$run(ctx)
  expect_true(kind %in% names(ctx$tables))
  expect_true(kind %in% names(ctx$plots))
}

test_that("step_sig_enrich_go generates results", {
  run_sig_enrich_step(step_sig_enrich_go, "go")
})

test_that("step_sig_enrich_kegg generates results", {
  run_sig_enrich_step(step_sig_enrich_kegg, "kegg")
})

test_that("step_sig_enrich_reactome generates results", {
  run_sig_enrich_step(step_sig_enrich_reactome, "reactome")
})

# ----- step_volcano -----
test_that("step_volcano generates plot", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma(),
    step_volcano()
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  # Should generate one plot per contrast or just one if single contrast?
  # step_volcano for limma generates plots for each contrast: volcano_Ref_vs_Test
  # Let's check if any plot starts with "volcano"
  expect_true(any(grepl("^volcano", names(res$plots))))
})

# ----- step_roc -----
test_that("step_roc generates results", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_roc())
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("roc_auc" %in% names(res$tables))
  expect_true("roc_curves" %in% names(res$plots))
})
