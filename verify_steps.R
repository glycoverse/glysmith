
library(devtools)
load_all()

# Verification function
check_step <- function(step, expected_id) {
  if (step$id != expected_id) {
    stop(paste("Mismatch for", expected_id, ": got", step$id))
  }
  print(paste("Verified", expected_id))
}

# PCA
check_step(step_pca("exp"), "pca")
check_step(step_pca("sig_exp"), "pca_sig")
check_step(step_pca("trait_exp"), "pca_trait")
check_step(step_pca("sig_trait_exp"), "pca_sig_trait")

# t-SNE
check_step(step_tsne("exp"), "tsne")
check_step(step_tsne("sig_exp"), "tsne_sig")
check_step(step_tsne("trait_exp"), "tsne_trait")
check_step(step_tsne("sig_trait_exp"), "tsne_sig_trait")

# UMAP
check_step(step_umap("exp"), "umap")
check_step(step_umap("sig_exp"), "umap_sig")
check_step(step_umap("trait_exp"), "umap_trait")
check_step(step_umap("sig_trait_exp"), "umap_sig_trait")

# Heatmap -- IDs changed!
check_step(step_heatmap("exp"), "heatmap")
check_step(step_heatmap("sig_exp"), "heatmap_sig")
check_step(step_heatmap("trait_exp"), "heatmap_trait")
check_step(step_heatmap("sig_trait_exp"), "heatmap_sig_trait")
