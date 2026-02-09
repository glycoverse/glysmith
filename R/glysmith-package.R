#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
#' @importFrom rlang .data
## usethis namespace: end
NULL

ignore_unused_imports <- function() {
  EnhancedVolcano::EnhancedVolcano
  clusterProfiler::enrichGO
  factoextra::fviz_pca_ind
  org.Hs.eg.db::org.Hs.eg.db
  pROC::plot.roc
  Rtsne::Rtsne
  uwot::umap
  ggplotify::as.ggplot
  pheatmap::pheatmap
}
