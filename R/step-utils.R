#' Check whether an experiment has glycan structures
#'
#' @param exp A glyco `SummarizedExperiment` object.
#'
#' @returns `TRUE` if variable information has a `glycan_structure` column.
#' @noRd
.has_glycan_structure <- function(exp) {
  "glycan_structure" %in% colnames(.get_var_info(exp))
}

# Fold-change consumers must use the scale of the fitted result. The active
# experiment may have been transformed since the model was fitted.
.dea_uses_logx <- function(res) {
  identical(res$preprocessing$input_scale %||% res$meta_data$scale, "logx")
}

.check_step_dots <- function(call) {
  if ("on" %in% names(call)) {
    cli::cli_abort(
      "Unknown step argument `on`. Connect the input with %=>%."
    )
  }
}
