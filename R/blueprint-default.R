#' Default blueprint
#'
#' This blueprint contains the following steps:
#' - step_ident_overview(): Summarize the experiment using `glyexp::summarize_experiment()`.
#' - step_preprocess(): Preprocess the data using `glyclean::auto_clean()`.
#' - step_plot_qc(when = "post"): Plot QC plots using `glyclean::plot_qc()`.
#' - step_pca(): Principal component analysis using `glystats::gly_pca()`,
#'   and plot the PCA using `glyvis::plot_pca()`.
#' - step_dea_limma(): Differential analysis using `glystats::gly_limma()`.
#' - step_volcano(): Plot a volcano plot using `glyvis::plot_volcano()`.
#' - step_heatmap(on = "sig_exp"): Plot a heatmap using `glyvis::plot_heatmap()`.
#' - step_sig_enrich_go(): Perform GO enrichment analysis using `glyfun::enrich_ora_go()`.
#' - step_sig_enrich_kegg(): Perform KEGG enrichment analysis using `glyfun::enrich_ora_kegg()`.
#' - step_sig_enrich_reactome(): Perform Reactome enrichment analysis using `glyfun::enrich_ora_reactome()`.
#' - step_derive_traits(): Derive traits using `glydet::derive_traits()`.
#' - step_dea_limma(on = "trait_exp"): Differential trait analysis using `glystats::gly_limma()`.
#' - step_heatmap(on = "sig_trait_exp"): Plot a heatmap using `glyvis::plot_heatmap()`.
#'
#' @param preprocess Whether to include [step_preprocess()].
#' @param enrich Whether to include the enrichment steps,
#'   i.e. [step_sig_enrich_go()], [step_sig_enrich_kegg()], and [step_sig_enrich_reactome()].
#' @param traits Whether to include the derived trait analysis steps,
#'   i.e. [step_derive_traits()] and `step_dea_limma(on = "trait_exp")`.
#'
#' @returns A `glysmith_blueprint` object.
#' @examples
#' blueprint_default()
#' @export
blueprint_default <- function(preprocess = TRUE, enrich = TRUE, traits = TRUE) {
  steps <- list(step_ident_overview())
  if (preprocess) {
    steps <- append(steps, list(step_preprocess()))
  }
  steps <- append(
    steps,
    list(
      step_plot_qc(when = "post"),
      step_pca(),
      step_dea_limma(),
      step_volcano(),
      step_heatmap("sig_exp")
    )
  )
  if (enrich) {
    steps <- append(
      steps,
      list(
        step_sig_enrich_go(),
        step_sig_enrich_kegg(),
        step_sig_enrich_reactome()
      )
    )
  }
  if (traits) {
    steps <- append(
      steps,
      list(
        step_derive_traits(),
        step_dea_limma(on = "trait_exp"),
        step_heatmap("sig_trait_exp")
      )
    )
  }
  names(steps) <- purrr::map_chr(steps, "id")
  new_blueprint(steps)
}
