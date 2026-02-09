#' Perform the Whole Analysis Pipeline
#'
#' This function performs a comprehensive analysis for group comparison.
#'
#' @param exp A `glyexp::experiment()` object.
#' @param blueprint A `glysmith_blueprint` object. Default is [blueprint_default()].
#' @param group_col Column name of group information in the sample information.
#'   Used for various analyses. Default is "group".
#'
#' @returns A `glysmith_result` object, with the following components:
#'   - `exp`: the experiment after preprocessing.
#'   - `plots`: a named list of ggplot objects.
#'   - `tables`: a named list of tibbles.
#'   - `meta`: a named list of metadata, containing:
#'     - `explanation`: a named character vector or list of explanations for each plot and table,
#'        with keys like `tables$summary` and `plots$pca`.
#'     - `steps`: a character vector of the steps of the analysis.
#'     - `log`: the messages and outputs from each step.
#'   - `blueprint`: the blueprint used for the analysis.
#'
#' @examples
#' exp <- glyexp::real_experiment2
#' result <- forge_analysis(exp)
#' print(result)
#'
#' @export
forge_analysis <- function(exp, blueprint = blueprint_default(), group_col = "group") {
  check_glysmith_deps(action = "error")
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col)
  if (!group_col %in% colnames(exp$sample_info)) {
    cli::cli_abort("Column name '{group_col}' is not found in the sample information.")
  }
  exp$sample_info[["group"]] <- droplevels(as.factor(exp$sample_info[[group_col]]))

  ctx <- new_ctx(exp, group_col)
  ctx <- run_blueprint(blueprint, ctx)

  exp <- ctx_get_data(ctx, "exp")
  glysmith_result(exp = exp, data = ctx$data, plots = ctx$plots, tables = ctx$tables, meta = ctx$meta, blueprint = blueprint)
}
