#' All steps in GlySmith
#'
#' @return A list of `glysmith_step` objects.
#' @noRd
all_steps <- function() {
  steps <- list(
    step_preprocess(),
    step_subset_groups(),
    step_adjust_protein(),
    step_ident_overview(),
    step_pca(),
    step_tsne(),
    step_umap(),
    step_heatmap(),
    step_logo(),
    step_dea_limma(),
    step_dea_ttest(),
    step_dea_wilcox(),
    step_dea_anova(),
    step_dea_kruskal(),
    step_volcano(),
    step_sig_enrich_go(),
    step_sig_enrich_kegg(),
    step_sig_enrich_reactome(),
    step_derive_traits(),
    step_quantify_motifs(),
    step_roc()
  )
  names(steps) <- purrr::map_chr(steps, "id")
  steps
}
