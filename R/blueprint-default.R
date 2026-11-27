#' Default group-comparison blueprint
#'
#' Summarize the input and analyze preprocessed abundances with QC, PCA and
#' limma. Downstream routes visualize the fit and the significant variables.
#' Optional trait analysis fits a separate model to the derived experiment.
#'
#' @param preprocess Whether to include [step_preprocess()].
#' @param enrich Whether to include the enrichment steps,
#'   i.e. [step_sig_enrich_go()], [step_sig_enrich_kegg()], and [step_sig_enrich_reactome()].
#' @param traits Whether to include the derived trait analysis steps,
#'   including [step_derive_traits()] and [step_dea_limma()].
#'
#' @returns A `glysmith_blueprint` object.
#' @examples
#' blueprint_default()
#' @export
blueprint_default <- function(preprocess = TRUE, enrich = TRUE, traits = TRUE) {
  consumers <- list(
    step_volcano(),
    blueprint(step_filter_sig_vars(), step_heatmap())
  )
  if (enrich) {
    consumers <- c(
      consumers,
      list(
        step_sig_enrich_go(),
        step_sig_enrich_kegg(),
        step_sig_enrich_reactome()
      )
    )
  }
  routes <- list(
    qc = step_plot_qc(when = "post"),
    scores = step_pca(),
    abundances = blueprint(step_dea_limma(), fork(!!!consumers))
  )
  if (traits) {
    routes$traits <- blueprint(
      step_derive_traits(),
      step_dea_limma(),
      step_filter_sig_vars(),
      step_heatmap()
    )
  }
  prep <- if (preprocess) list(step_preprocess()) else list()
  blueprint(br("overview", step_ident_overview()), !!!prep, fork(!!!routes))
}
