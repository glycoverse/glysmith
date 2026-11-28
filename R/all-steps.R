#' All steps in GlySmith
#'
#' @return A list of `glysmith_step` objects.
#' @noRd
all_steps <- function() {
  steps <- list(
    step_ident_overview(),
    step_plot_qc(),
    step_preprocess(),
    step_zero_to_na(),
    step_remove(),
    step_normalize(),
    step_impute(),
    step_aggregate(),
    step_detect_batch_effect(),
    step_correct_batch_effect(),
    step_transform_log(),
    step_subset_groups(),
    step_adjust_protein(),
    step_coda(),
    step_correlation(),
    step_pca(),
    step_tsne(),
    step_umap(),
    step_oplsda(),
    step_plsda(),
    step_heatmap(),
    step_plot_glycosite_profile(),
    step_logo(),
    step_sig_boxplot(),
    step_dea_limma(),
    step_dea_ttest(),
    step_dea_wilcox(),
    step_dea_anova(),
    step_dea_kruskal(),
    step_cox(),
    step_volcano(),
    step_filter_sig_vars(),
    step_sig_enrich_go(),
    step_sig_enrich_kegg(),
    step_sig_enrich_reactome(),
    step_sig_enrich_ncg(),
    step_sig_enrich_wp(),
    step_sig_enrich_do(),
    step_infer_structure(),
    step_derive_traits(),
    step_quantify_motifs(),
    step_quantify_dynamic_motifs(),
    step_quantify_branch_motifs(),
    step_roc()
  )
  names(steps) <- purrr::map_chr(steps, "id")
  steps
}
