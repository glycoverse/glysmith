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
#' @param blueprint A `glysmith_blueprint` object. Default is [blueprint_default()].
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
#'     - `explanation`: a named character vector or list of explanations for each plot and table,
#'        with keys like `tables$summary` and `plots$pca`.
#'     - `steps`: a character vector of the steps of the analysis.
#'
#' @examples
#' exp <- glyexp::real_experiment2
#' result <- forge_analysis(exp)
#' print(result)
#'
#' @export
forge_analysis <- function(exp, blueprint = blueprint_default(), group_col = "group", ...) {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col)
  if (!group_col %in% colnames(exp$sample_info)) {
    cli::cli_abort("Column name '{group_col}' is not found in the sample information.")
  }
  exp$sample_info[["group"]] <- droplevels(as.factor(exp$sample_info[[group_col]]))

  dots <- rlang::list2(...)
  ctx <- new_ctx(exp, group_col, dots)
  ctx <- run_blueprint(blueprint, ctx)

  exp <- ctx_get_data(ctx, "clean_exp")
  glysmith_result(exp = exp, plots = ctx$plots, tables = ctx$tables, meta = ctx$meta, blueprint = blueprint)
}
