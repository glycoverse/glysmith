#' Forge an Analysis for Group Comparison
#'
#' @description
#' This function performs comprehensive analysis for group comparison.
#'
#' The pipeline includes:
#' - Preprocessing using `glyclean::auto_clean()`
#' - Identification overview using `glyexp::summarize_experiment()`
#' - Principal component analysis (PCA) with `glystats::gly_pca()` and `glyvis::plot_pca()`
#' - Differential expression analysis (DEA) with `glystats::gly_limma()` and `glyvis::plot_volcano()`
#'
#' If experiment type is "glycoproteomics", the pipeline will also include:
#' - Functional enrichment analysis using `glystats::gly_enrich_go()`, `glystats::gly_enrich_kegg()`,
#'   and `glystats::gly_enrich_reactome()`, as well as plotting with `glyvis::plot_enrich()`
#'
#' If glycan structure is available and glycan type is "N",
#' the pipeline will also include:
#' - Derived trait calculation using `glydet::derive_traits()`
#' - Differential trait analysis (DTA) with `glystats::gly_limma()`
#'
#' @param exp A `glyexp::experiment()` object.
#' @param group_col Column name of group information in the sample information.
#'   Used for various analyses. Default is "group".
#' @param ... Additional arguments passed to the functions.
#'   Use the format `pkg.func.arg` to pass arguments to the functions.
#'   For example, if you want to pass argument `p_adj_method = "BH"` to `glystats::gly_limma()`,
#'   set `glystats.gly_limma.p_adj_method = "BH"`.
#'   Note that arguments about group column specification is controlled by `group_col` argument,
#'   and should not be passed to `...`.
#'
#' @returns A `glysmith_result` object, with the following components:
#'   - `exp`: the experiment after preprocessing.
#'   - `plots`: a named list of ggplot objects.
#'   - `tables`: a named list of tibbles.
#'   - `meta`: a named list of metadata. Currently two elements:
#'     - `explanation`: a character vector of the explanation of each plot and table,
#'        with the same names as `plots` and `tables`.
#'     - `steps`: a character vector of the steps of the analysis.
#'
#' @examples
#' exp <- glyexp::real_experiment
#' result <- forge_analysis(exp)
#' print(result)
#'
#' @export
forge_analysis <- function(exp, group_col = "group", ...) {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col)
  if (!group_col %in% colnames(exp$sample_info)) {
    cli::cli_abort("Column name '{group_col}' is not found in the sample information.")
  }
  exp$sample_info[[group_col]] <- droplevels(as.factor(exp$sample_info[[group_col]]))

  n_groups <- length(levels(exp$sample_info[[group_col]]))
  has_structure <- "glycan_structure" %in% colnames(exp$var_info)
  is_gp <- glyexp::get_exp_type(exp) == "glycoproteomics"

  dots <- rlang::list2(...)

  plots <- list()
  tables <- list()
  meta <- list(explanation = character(0), steps = character(0))

  # ========== Preprocessing ==========
  cli::cli_progress_step("Preprocessing")
  exp <- .run_function(glyclean::auto_clean, exp, group_col, dots, "group_col")
  meta$steps <- c(meta$steps, "preprocessing")

  # ========== Identification overview ==========
  cli::cli_progress_step("Identification overview")
  tables$summary <- .run_function(glyexp::summarize_experiment, exp, group_col, dots)
  meta$explanation[["tables$summary"]] <- "Identification overview of the experiment."
  meta$steps <- c(meta$steps, "identification overview")

  # ========== Principal component analysis ==========
  cli::cli_progress_step("Principal component analysis")
  pca_res <- .run_function(glystats::gly_pca, exp, group_col, dots)
  tables$pca_samples <- glystats::get_tidy_result(pca_res, "samples")
  tables$pca_variables <- glystats::get_tidy_result(pca_res, "variables")
  tables$pca_eigenvalues <- glystats::get_tidy_result(pca_res, "eigenvalues")
  plots$pca <- .run_function(glyvis::plot_pca, pca_res, group_col, dots, "group_col")
  meta$explanation[["tables$pca_samples"]] <- "PCA scores for each sample."
  meta$explanation[["tables$pca_variables"]] <- "PCA loadings for each variable."
  meta$explanation[["tables$pca_eigenvalues"]] <- "PCA eigenvalues."
  meta$steps <- c(meta$steps, "pca analysis")

  # ========== Differential expression analysis ==========
  cli::cli_progress_step("Differential expression analysis")
  dea_res <- .run_function(glystats::gly_limma, exp, group_col, dots)
  tables$dea <- glystats::get_tidy_result(dea_res)
  meta$explanation[["tables$dea"]] <- "Differential expression analysis results of all comparisons for all variables."
  meta$steps <- c(meta$steps, "differential expression analysis")
  if (n_groups == 2) {
    plots$volcano <- .run_function(glyvis::plot_volcano, dea_res, group_col, dots)
    meta$explanation[["plots$volcano"]] <- "Volcano plot for the comparison of the two groups."
    meta$steps <- c(meta$steps, "volcano plot")
  }

  if (is_gp) {
    cli::cli_progress_step("GO enrichment analysis")
    go_res <- .run_function(glystats::gly_enrich_go, exp, group_col, dots)
    tables$go <- glystats::get_tidy_result(go_res)
    plots$go <- .run_function(glyvis::plot_enrich, go_res, group_col, dots)
    meta$explanation[["tables$go"]] <- "GO enrichment analysis results."
    meta$explanation[["plots$go"]] <- "GO enrichment analysis plot."
    meta$steps <- c(meta$steps, "go enrichment analysis")

    cli::cli_progress_step("KEGG enrichment analysis")
    kegg_res <- .run_function(glystats::gly_enrich_kegg, exp, group_col, dots)
    tables$kegg <- glystats::get_tidy_result(kegg_res)
    plots$kegg <- .run_function(glyvis::plot_enrich, kegg_res, group_col, dots)
    meta$explanation[["tables$kegg"]] <- "KEGG enrichment analysis results."
    meta$explanation[["plots$kegg"]] <- "KEGG enrichment analysis plot."
    meta$steps <- c(meta$steps, "kegg enrichment analysis")

    cli::cli_progress_step("Reactome enrichment analysis")
    reactome_res <- .run_function(glystats::gly_enrich_reactome, exp, group_col, dots)
    tables$reactome <- glystats::get_tidy_result(reactome_res)
    plots$reactome <- .run_function(glyvis::plot_enrich, reactome_res, group_col, dots)
    meta$explanation[["tables$reactome"]] <- "Reactome enrichment analysis results."
    meta$explanation[["plots$reactome"]] <- "Reactome enrichment analysis plot."
    meta$steps <- c(meta$steps, "reactome enrichment analysis")
  }

  if (has_structure) {
    cli::cli_progress_step("Derived trait calculation")
    trait_exp <- .run_function(glydet::derive_traits, exp, group_col, dots)
    tables$derived_traits <- tibble::as_tibble(trait_exp)
    meta$explanation[["tables$derived_traits"]] <- "Derived trait calculation results."
    meta$steps <- c(meta$steps, "derived trait calculation")

    cli::cli_progress_step("Differential trait analysis")
    suppressMessages(filtered_trait_exp <- glyclean::remove_constant(trait_exp))
    dta_res <- .run_function(glystats::gly_limma, filtered_trait_exp, group_col, dots)
    tables$dta <- glystats::get_tidy_result(dta_res)
    meta$explanation[["tables$dta"]] <- "Differential trait analysis results."
    meta$steps <- c(meta$steps, "differential trait analysis")
  }

  glysmith_result(exp = exp, plots = plots, tables = tables, meta = meta)
}