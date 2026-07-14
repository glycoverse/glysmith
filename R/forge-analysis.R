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
#' \dontrun{
#' exp <- glyexp::real_experiment2
#' result <- forge_analysis(exp)
#' print(result)
#' }
#'
#' @export
forge_analysis <- function(
  exp,
  blueprint = blueprint_default(),
  group_col = "group"
) {
  check_glysmith_deps(blueprint = blueprint, action = "error")
  .assert_data_container(exp)
  checkmate::assert_string(group_col)
  legacy <- inherits(exp, "glyexp_experiment")
  exp <- .as_glyco_se(exp)
  sample_info <- .get_sample_info(exp)
  if (!group_col %in% colnames(sample_info)) {
    cli::cli_abort(
      "Column name '{group_col}' is not found in the sample information."
    )
  }
  sample_info[["group"]] <- droplevels(as.factor(sample_info[[
    group_col
  ]]))
  exp <- .set_sample_info(exp, sample_info)

  ctx <- new_ctx(exp, group_col)
  ctx <- run_blueprint(blueprint, ctx)

  ctx$data <- purrr::map(
    ctx$data,
    .restore_data_container,
    legacy = legacy
  )
  exp <- ctx_get_data(ctx, "exp")
  glysmith_result(
    exp = exp,
    data = ctx$data,
    plots = ctx$plots,
    tables = ctx$tables,
    meta = ctx$meta,
    blueprint = blueprint
  )
}
