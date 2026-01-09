#' Step: Principal Component Analysis (PCA)
#'
#' Run PCA using `glystats::gly_pca()` and plot it with `glyvis::plot_pca()`.
#' Loading plot for glycoproteomics data can be crowded with too many variables.
#' Ignore the resulting plot if it is not informative.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to run PCA on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run PCA on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to run PCA on
#'
#' Tables generated (with suffixes):
#' - `pca_samples`: A table containing the PCA scores for each sample
#' - `pca_variables`: A table containing the PCA loadings for each variable
#' - `pca_eigenvalues`: A table containing the PCA eigenvalues
#'
#' Plots generated (with suffixes):
#' - `pca_scores`: A PCA score plot colored by group
#' - `pca_loadings`: A PCA loading plot
#' - `pca_screeplot`: A PCA screeplot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#'
#' @param on Name of the experiment to run PCA on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
#' @inheritParams glystats::gly_pca
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_pca()
#' @seealso [glystats::gly_pca()], [glyvis::plot_pca()]
#' @export
step_pca <- function(
  on = "exp",
  center = TRUE,
  scale = TRUE,
  ...
) {
  rlang::check_installed("factoextra")
  signature <- rlang::expr_deparse(match.call())
  pca_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("pca", on_meta$id_suffix)
  step(
    id = id,
    label = paste0("Principal component analysis", on_meta$label_suffix),
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      pca_res <- rlang::exec(
        glystats::gly_pca,
        exp,
        center = center,
        scale = scale,
        !!!pca_args
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_samples"),
        glystats::get_tidy_result(pca_res, "samples"),
        paste0("PCA scores for each sample of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_variables"),
        glystats::get_tidy_result(pca_res, "variables"),
        paste0("PCA loadings for each variable of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_eigenvalues"),
        glystats::get_tidy_result(pca_res, "eigenvalues"),
        paste0("PCA eigenvalues of ", on, ".")
      )
      p_scores <- glyvis::plot_pca(pca_res, type = "individual")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_scores"),
        p_scores,
        paste0("PCA score plot colored by group of ", on, ".")
      )
      p_loadings <- glyvis::plot_pca(pca_res, type = "variables")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_loadings"),
        p_loadings,
        paste0("PCA loading plot of ", on, ".")
      )
      p_screeplot <- glyvis::plot_pca(pca_res, type = "screeplot")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_screeplot"),
        p_screeplot,
        paste0("PCA screeplot of ", on, ".")
      )
      ctx
    },
    require = on,
    signature = signature
  )
}

#' Step: t-SNE
#'
#' Perform t-SNE analysis using `glystats::gly_tsne()` and
#' plot a t-SNE plot using `glyvis::plot_tsne()`.
#' Note that the result of t-SNE largely depends on the `perplexity` parameter.
#' Usually it's a trial-and-error process to find the best value iteratively.
#' If you are not satisfied with the result,
#' manually call `glyvis::plot_tsne()` with different `perplexity` values
#' to find the best one.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to perform t-SNE on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform t-SNE on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to perform t-SNE on
#'
#' Data generated (with suffixes):
#' - `tsne`: The t-SNE result
#'
#' Plots generated (with suffixes):
#' - `tsne`: The t-SNE plot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only when the user explicitly asks for t-SNE.
#'
#' @param on Name of the experiment to run t-SNE on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
#' @inheritParams glystats::gly_tsne
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_tsne()
#' step_tsne(perplexity = 30)
#' @seealso [glystats::gly_tsne()], [glyvis::plot_tsne()]
#' @export
step_tsne <- function(
  on = "exp",
  dims = 2,
  perplexity = 30,
  ...
) {
  rlang::check_installed("Rtsne")
  signature <- rlang::expr_deparse(match.call())
  tsne_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("tsne", on_meta$id_suffix)

  step(
    id = id,
    label = "t-SNE",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      tsne <- rlang::exec(
        glystats::gly_tsne,
        exp,
        dims = dims,
        perplexity = perplexity,
        !!!tsne_args
      )
      ctx <- ctx_add_table(ctx, id, glystats::get_tidy_result(tsne), paste0("t-SNE result of ", on, "."))
      p <- glyvis::plot_tsne(tsne)
      ctx <- ctx_add_plot(ctx, id, p, paste0("t-SNE plot of ", on, "."))
      ctx
    },
    require = on,
    signature = signature
  )
}

#' Step: UMAP
#'
#' Perform UMAP analysis using `glystats::gly_umap()` and
#' plot a UMAP plot using `glyvis::plot_umap()`.
#' Note that the result of UMAP largely depends on the `n_neighbors` parameter.
#' Usually it's a trial-and-error process to find the best value iteratively.
#' If you are not satisfied with the result,
#' manually call `glyvis::plot_umap()` with different `n_neighbors` values
#' to find the best one.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to perform UMAP on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform UMAP on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to perform UMAP on
#'
#' Data generated (with suffixes):
#' - `umap`: The UMAP result
#'
#' Plots generated (with suffixes):
#' - `umap`: The UMAP plot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only when the user explicitly asks for UMAP.
#'
#' @param on Name of the experiment to run UMAP on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
#' @inheritParams glystats::gly_umap
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_umap()
#' step_umap(n_neighbors = 15)
#' @seealso [glystats::gly_umap()], [glyvis::plot_umap()]
#' @export
step_umap <- function(
  on = "exp",
  n_neighbors = 15,
  n_components = 2,
  ...
) {
  rlang::check_installed("uwot")
  signature <- rlang::expr_deparse(match.call())
  umap_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("umap", on_meta$id_suffix)

  step(
    id = id,
    label = "UMAP",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      umap <- rlang::exec(
        glystats::gly_umap,
        exp,
        n_neighbors = n_neighbors,
        n_components = n_components,
        !!!umap_args
      )
      ctx <- ctx_add_table(ctx, id, glystats::get_tidy_result(umap), paste0("UMAP result of ", on, "."))
      p <- glyvis::plot_umap(umap)
      ctx <- ctx_add_plot(ctx, id, p, paste0("UMAP plot of ", on, "."))
      ctx
    },
    require = on,
    signature = signature
  )
}
