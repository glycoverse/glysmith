#' Step: Principal Component Analysis (PCA)
#'
#' @description
#' Run PCA using `glystats::gly_pca()` and plot it with `glyvis::plot_pca()`.
#' Loading plot for glycoproteomics data can be crowded with too many variables.
#' Ignore the resulting plot if it is not informative.
#'
#' This step depends on the `on` parameter (default: `exp`).
#' - When `on = "exp"`, requires `exp` (usually after [step_preprocess()]).
#' - When `on = "sig_exp"`, requires `sig_exp` from one of [step_dea_limma()],
#'   [step_dea_ttest()], [step_dea_wilcox()], [step_dea_anova()], or [step_dea_kruskal()].
#' - When `on = "trait_exp"`, requires `trait_exp` from [step_derive_traits()].
#' - When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on traits.
#' - When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from [step_quantify_dynamic_motifs()].
#' - When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp` from DEA on motifs.
#' - When `on = "branch_motif_exp"`, requires `branch_motif_exp` from [step_quantify_branch_motifs()].
#' - When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp` from DEA on motifs.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to run PCA on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run PCA on
#' - `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif experiment to run PCA on
#' - `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif experiment to run PCA on
#'
#' Tables generated (with suffixes):
#' - `pca_samples`: A table containing the PCA scores for each sample
#' - `pca_variables`: A table containing the PCA loadings for each variable
#' - `pca_eigenvalues`: A table containing the PCA eigenvalues
#'
#' Plots generated (with suffixes):
#' - `pca_scores`: A PCA score plot colored by group (always generated)
#' - `pca_loadings`: A PCA loading plot (if `loadings = TRUE`)
#' - `pca_screeplot`: A PCA screeplot (if `screeplot = TRUE`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#'
#' @param on Name of the experiment to run PCA on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp",
#'   "dynamic_motif_exp", "sig_dynamic_motif_exp",
#'   "branch_motif_exp", "sig_branch_motif_exp".
#' @param plot_width Width of plots in inches. Default is 5.
#' @param plot_height Height of plots in inches. Default is 5.
#' @param loadings Logical indicating whether to generate the loading plot.
#'   Default is `FALSE` since loading plots for glycoproteomics data can be crowded.
#' @param screeplot Logical indicating whether to generate the screeplot.
#'   Default is `TRUE`.
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
  loadings = FALSE,
  screeplot = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
) {
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
        paste0("PCA score plot colored by group of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      if (loadings) {
        p_loadings <- glyvis::plot_pca(pca_res, type = "variables")
        ctx <- ctx_add_plot(
          ctx,
          paste0(id, "_loadings"),
          p_loadings,
          paste0("PCA loading plot of ", on, "."),
          width = plot_width,
          height = plot_height
        )
      }
      if (screeplot) {
        p_screeplot <- glyvis::plot_pca(pca_res, type = "screeplot")
        ctx <- ctx_add_plot(
          ctx,
          paste0(id, "_screeplot"),
          p_screeplot,
          paste0("PCA screeplot of ", on, "."),
          width = plot_width,
          height = plot_height
        )
      }
      ctx
    },
    require = on,
    signature = signature
  )
}

#' Step: t-SNE
#'
#' @description
#' Perform t-SNE analysis using `glystats::gly_tsne()` and
#' plot a t-SNE plot using `glyvis::plot_tsne()`.
#' Note that the result of t-SNE largely depends on the `perplexity` parameter.
#' Usually it's a trial-and-error process to find the best value iteratively.
#' If you are not satisfied with the result,
#' manually call `glyvis::plot_tsne()` with different `perplexity` values
#' to find the best one.
#'
#' This step depends on the `on` parameter (default: `exp`).
#' - When `on = "exp"`, requires `exp` (usually after [step_preprocess()]).
#' - When `on = "sig_exp"`, requires `sig_exp` from one of [step_dea_limma()],
#'   [step_dea_ttest()], [step_dea_wilcox()], [step_dea_anova()], or [step_dea_kruskal()].
#' - When `on = "trait_exp"`, requires `trait_exp` from [step_derive_traits()].
#' - When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on traits.
#' - When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from [step_quantify_dynamic_motifs()].
#' - When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp` from DEA on motifs.
#' - When `on = "branch_motif_exp"`, requires `branch_motif_exp` from [step_quantify_branch_motifs()].
#' - When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp` from DEA on motifs.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to perform t-SNE on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform t-SNE on
#' - `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif experiment to perform t-SNE on
#' - `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif experiment to perform t-SNE on
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
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp",
#'   "dynamic_motif_exp", "sig_dynamic_motif_exp",
#'   "branch_motif_exp", "sig_branch_motif_exp".
#' @param plot_width Width of the plot in inches. Default is 5.
#' @param plot_height Height of the plot in inches. Default is 5.
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
  plot_width = 5,
  plot_height = 5,
  ...
) {
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
      ctx <- ctx_add_table(
        ctx,
        id,
        glystats::get_tidy_result(tsne),
        paste0("t-SNE result of ", on, ".")
      )
      p <- glyvis::plot_tsne(tsne)
      ctx <- ctx_add_plot(
        ctx,
        id,
        p,
        paste0("t-SNE plot of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      ctx
    },
    require = on,
    signature = signature
  )
}

#' Step: UMAP
#'
#' @description
#' Perform UMAP analysis using `glystats::gly_umap()` and
#' plot a UMAP plot using `glyvis::plot_umap()`.
#' Note that the result of UMAP largely depends on the `n_neighbors` parameter.
#' Usually it's a trial-and-error process to find the best value iteratively.
#' If you are not satisfied with the result,
#' manually call `glyvis::plot_umap()` with different `n_neighbors` values
#' to find the best one.
#'
#' This step depends on the `on` parameter (default: `exp`).
#' - When `on = "exp"`, requires `exp` (usually after [step_preprocess()]).
#' - When `on = "sig_exp"`, requires `sig_exp` from one of [step_dea_limma()],
#'   [step_dea_ttest()], [step_dea_wilcox()], [step_dea_anova()], or [step_dea_kruskal()].
#' - When `on = "trait_exp"`, requires `trait_exp` from [step_derive_traits()].
#' - When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on traits.
#' - When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from [step_quantify_dynamic_motifs()].
#' - When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp` from DEA on motifs.
#' - When `on = "branch_motif_exp"`, requires `branch_motif_exp` from [step_quantify_branch_motifs()].
#' - When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp` from DEA on motifs.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to perform UMAP on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform UMAP on
#' - `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif experiment to perform UMAP on
#' - `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif experiment to perform UMAP on
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
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp",
#'   "dynamic_motif_exp", "sig_dynamic_motif_exp",
#'   "branch_motif_exp", "sig_branch_motif_exp".
#' @param plot_width Width of the plot in inches. Default is 5.
#' @param plot_height Height of the plot in inches. Default is 5.
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
  plot_width = 5,
  plot_height = 5,
  ...
) {
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
      ctx <- ctx_add_table(
        ctx,
        id,
        glystats::get_tidy_result(umap),
        paste0("UMAP result of ", on, ".")
      )
      p <- glyvis::plot_umap(umap)
      ctx <- ctx_add_plot(
        ctx,
        id,
        p,
        paste0("UMAP plot of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      ctx
    },
    require = on,
    signature = signature
  )
}

#' Step: Partial Least Squares Discriminant Analysis (PLS-DA)
#'
#' @description
#' Perform PLS-DA using `glystats::gly_plsda()` and plot it with `glyvis::plot_plsda()`.
#' PLS-DA is a supervised method that finds components maximizing covariance between
#' predictors and the response variable (group membership).
#'
#' This step depends on the `on` parameter (default: `exp`).
#' - When `on = "exp"`, requires `exp` (usually after [step_preprocess()]).
#' - When `on = "sig_exp"`, requires `sig_exp` from one of [step_dea_limma()],
#'   [step_dea_ttest()], [step_dea_wilcox()], [step_dea_anova()], or [step_dea_kruskal()].
#' - When `on = "trait_exp"`, requires `trait_exp` from [step_derive_traits()].
#' - When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on traits.
#' - When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from [step_quantify_dynamic_motifs()].
#' - When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp` from DEA on motifs.
#' - When `on = "branch_motif_exp"`, requires `branch_motif_exp` from [step_quantify_branch_motifs()].
#' - When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp` from DEA on motifs.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to run PLS-DA on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run PLS-DA on
#' - `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif experiment to run PLS-DA on
#' - `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif experiment to run PLS-DA on
#'
#' Tables generated (with suffixes):
#' - `plsda_samples`: A table containing the PLS-DA scores for each sample
#' - `plsda_variables`: A table containing the PLS-DA loadings for each variable
#' - `plsda_variance`: A table containing the explained variance for each component
#' - `plsda_vip`: A table containing the Variable Importance in Projection (VIP) scores
#' - `plsda_perm_test`: A table containing permutation test results
#'
#' Plots generated (with suffixes):
#' - `plsda_scores`: A PLS-DA score plot colored by group
#' - `plsda_loadings`: A PLS-DA loading plot
#' - `plsda_variance`: A PLS-DA variance (scree) plot
#' - `plsda_vip`: A PLS-DA VIP score plot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step when users explicitly asks for PLS-DA.
#'
#' @param on Name of the experiment to run PLS-DA on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp",
#'   "dynamic_motif_exp", "sig_dynamic_motif_exp",
#'   "branch_motif_exp", "sig_branch_motif_exp".
#' @param ncomp Number of components to include. Default is 2.
#' @param scale Logical indicating whether to scale the data. Default is TRUE.
#' @param plot_width Width of plots in inches. Default is 5.
#' @param plot_height Height of plots in inches. Default is 5.
#' @param ... Additional arguments passed to `glystats::gly_plsda()`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_plsda()
#' step_plsda(ncomp = 3)
#' @seealso [glystats::gly_plsda()], [glyvis::plot_plsda()]
#' @export
step_plsda <- function(
  on = "exp",
  ncomp = 2,
  scale = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
) {
  signature <- rlang::expr_deparse(match.call())
  plsda_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("plsda", on_meta$id_suffix)

  step(
    id = id,
    label = paste0(
      "Partial least squares discriminant analysis",
      on_meta$label_suffix
    ),
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      plsda_res <- rlang::exec(
        glystats::gly_plsda,
        exp,
        ncomp = ncomp,
        scale = scale,
        !!!plsda_args
      )
      tidy_res <- plsda_res$tidy_result

      ctx <- ctx_add_data(ctx, paste0(id, "_raw_res"), plsda_res$raw_result)
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_samples"),
        tidy_res$samples,
        paste0("PLS-DA scores for each sample of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_variables"),
        tidy_res$variables,
        paste0("PLS-DA loadings for each variable of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_variance"),
        tidy_res$variance,
        paste0("PLS-DA explained variance of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_vip"),
        tidy_res$vip,
        paste0("PLS-DA VIP scores of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_perm_test"),
        tidy_res$perm_test,
        paste0("PLS-DA permutation test results of ", on, ".")
      )

      p_scores <- glyvis::plot_plsda(plsda_res, type = "scores")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_scores"),
        p_scores,
        paste0("PLS-DA score plot colored by group of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      p_loadings <- glyvis::plot_plsda(plsda_res, type = "loadings")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_loadings"),
        p_loadings,
        paste0("PLS-DA loading plot of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      p_variance <- glyvis::plot_plsda(plsda_res, type = "variance")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_variance"),
        p_variance,
        paste0("PLS-DA variance plot of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      p_vip <- glyvis::plot_plsda(plsda_res, type = "vip")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_vip"),
        p_vip,
        paste0("PLS-DA VIP score plot of ", on, "."),
        width = plot_width,
        height = plot_height
      )

      ctx
    },
    require = on,
    signature = signature,
    report = function(x) {
      raw_res <- x$data[[paste0(id, "_raw_res")]]
      model_df <- raw_res@modelDF
      r2y <- model_df[["R2Y"]][1]
      r2y_cum <- model_df[["R2Y(cum)"]][1]
      q2 <- model_df[["Q2"]][1]
      q2_cum <- model_df[["Q2(cum)"]][1]

      vip_tbl <- x$tables[[paste0(id, "_vip")]]
      n_vip_gt_1 <- sum(.data$vip > 1, na.rm = TRUE)

      top5 <- vip_tbl |>
        dplyr::arrange(dplyr::desc(.data$vip)) |>
        dplyr::slice_head(n = 5)
      top5_vars <- top5$variable

      lines <- c(
        paste0(
          "Partial least squares discriminant analysis (PLS-DA) was performed."
        ),
        paste0(
          "Model fit: R2Y = ",
          round(r2y, 3),
          " (cumulative: ",
          round(r2y_cum, 3),
          "), Q2 = ",
          round(q2, 3),
          " (cumulative: ",
          round(q2_cum, 3),
          ")."
        ),
        paste0("Number of variables with VIP > 1: ", n_vip_gt_1, "."),
        paste0(
          "Top 5 discriminant variables: ",
          paste(top5_vars, collapse = ", "),
          "."
        )
      )
      paste(lines, collapse = "\n")
    }
  )
}

#' Step: Orthogonal Partial Least Squares Discriminant Analysis (OPLS-DA)
#'
#' @description
#' Perform OPLS-DA using `glystats::gly_oplsda()` and plot it with `glyvis::plot_oplsda()`.
#' OPLS-DA separates variation into predictive (related to group) and orthogonal (unrelated) components.
#' This step only works with binary classification (exactly 2 groups).
#'
#' This step depends on the `on` parameter (default: `exp`).
#' - When `on = "exp"`, requires `exp` (usually after [step_preprocess()]).
#' - When `on = "sig_exp"`, requires `sig_exp` from one of [step_dea_limma()],
#'   [step_dea_ttest()], [step_dea_wilcox()], [step_dea_anova()], or [step_dea_kruskal()].
#' - When `on = "trait_exp"`, requires `trait_exp` from [step_derive_traits()].
#' - When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on traits.
#' - When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from [step_quantify_dynamic_motifs()].
#' - When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp` from DEA on motifs.
#' - When `on = "branch_motif_exp"`, requires `branch_motif_exp` from [step_quantify_branch_motifs()].
#' - When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp` from DEA on motifs.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to run OPLS-DA on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run OPLS-DA on
#' - `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif experiment to run OPLS-DA on
#' - `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif experiment to run OPLS-DA on
#'
#' Tables generated (with suffixes):
#' - `oplsda_samples`: A table containing the OPLS-DA scores for each sample
#' - `oplsda_variables`: A table containing the OPLS-DA loadings for each variable
#' - `oplsda_variance`: A table containing the explained variance for each component
#' - `oplsda_vip`: A table containing the Variable Importance in Projection (VIP) scores
#' - `oplsda_perm_test`: A table containing permutation test results
#'
#' Plots generated (with suffixes):
#' - `oplsda_scores`: An OPLS-DA score plot colored by group
#' - `oplsda_loadings`: An OPLS-DA loading plot
#' - `oplsda_variance`: An OPLS-DA variance (scree) plot
#' - `oplsda_vip`: An OPLS-DA VIP score plot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step when users explicitly asks for OPLS-DA.
#' - This step only works with binary classification (exactly 2 groups).
#'   If multiple groups are found, ask if `step_subset_groups()` should be run first.
#'
#' @param on Name of the experiment to run OPLS-DA on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp",
#'   "dynamic_motif_exp", "sig_dynamic_motif_exp",
#'   "branch_motif_exp", "sig_branch_motif_exp".
#' @param pred_i Number of predictive components to include. Default is 1.
#' @param ortho_i Number of orthogonal components to include. Default is NA (automatic).
#' @param scale Logical indicating whether to scale the data. Default is TRUE.
#' @param plot_width Width of plots in inches. Default is 5.
#' @param plot_height Height of plots in inches. Default is 5.
#' @param ... Additional arguments passed to `glystats::gly_oplsda()`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_oplsda()
#' step_oplsda(pred_i = 1, ortho_i = 1)
#' @seealso [glystats::gly_oplsda()], [glyvis::plot_oplsda()]
#' @export
step_oplsda <- function(
  on = "exp",
  pred_i = 1,
  ortho_i = NA,
  scale = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
) {
  signature <- rlang::expr_deparse(match.call())
  oplsda_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("oplsda", on_meta$id_suffix)

  step(
    id = id,
    label = paste0(
      "Orthogonal partial least squares discriminant analysis",
      on_meta$label_suffix
    ),
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      oplsda_res <- rlang::exec(
        glystats::gly_oplsda,
        exp,
        pred_i = pred_i,
        ortho_i = ortho_i,
        scale = scale,
        !!!oplsda_args
      )
      tidy_res <- oplsda_res$tidy_result

      ctx <- ctx_add_data(ctx, paste0(id, "_raw_res"), oplsda_res$raw_result)
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_samples"),
        tidy_res$samples,
        paste0("OPLS-DA scores for each sample of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_variables"),
        tidy_res$variables,
        paste0("OPLS-DA loadings for each variable of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_variance"),
        tidy_res$variance,
        paste0("OPLS-DA explained variance of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_vip"),
        tidy_res$vip,
        paste0("OPLS-DA VIP scores of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_perm_test"),
        tidy_res$perm_test,
        paste0("OPLS-DA permutation test results of ", on, ".")
      )

      p_scores <- glyvis::plot_oplsda(oplsda_res, type = "scores")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_scores"),
        p_scores,
        paste0("OPLS-DA score plot colored by group of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      p_loadings <- glyvis::plot_oplsda(oplsda_res, type = "loadings")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_loadings"),
        p_loadings,
        paste0("OPLS-DA loading plot of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      p_variance <- glyvis::plot_oplsda(oplsda_res, type = "variance")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_variance"),
        p_variance,
        paste0("OPLS-DA variance plot of ", on, "."),
        width = plot_width,
        height = plot_height
      )
      p_vip <- glyvis::plot_oplsda(oplsda_res, type = "vip")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_vip"),
        p_vip,
        paste0("OPLS-DA VIP score plot of ", on, "."),
        width = plot_width,
        height = plot_height
      )

      ctx
    },
    require = on,
    signature = signature,
    report = function(x) {
      raw_res <- x$data[[paste0(id, "_raw_res")]]
      model_df <- raw_res@modelDF
      r2y <- model_df[["R2Y"]][1]
      r2y_cum <- model_df[["R2Y(cum)"]][1]
      q2 <- model_df[["Q2"]][1]
      q2_cum <- model_df[["Q2(cum)"]][1]

      vip_tbl <- x$tables[[paste0(id, "_vip")]]
      n_vip_gt_1 <- sum(.data$vip > 1, na.rm = TRUE)

      top5 <- vip_tbl |>
        dplyr::arrange(dplyr::desc(.data$vip)) |>
        dplyr::slice_head(n = 5)
      top5_vars <- top5$variable

      lines <- c(
        paste0(
          "Orthogonal partial least squares discriminant analysis (OPLS-DA) was performed."
        ),
        paste0(
          "Model fit: R2Y = ",
          round(r2y, 3),
          " (cumulative: ",
          round(r2y_cum, 3),
          "), Q2 = ",
          round(q2, 3),
          " (cumulative: ",
          round(q2_cum, 3),
          ")."
        ),
        paste0("Number of variables with VIP > 1: ", n_vip_gt_1, "."),
        paste0(
          "Top 5 discriminant variables: ",
          paste(top5_vars, collapse = ", "),
          "."
        )
      )
      paste(lines, collapse = "\n")
    },
    condition = function(ctx) {
      exp <- tryCatch(ctx_get_data(ctx, on), error = function(e) NULL)
      if (is.null(exp)) {
        return(list(check = FALSE, reason = "Experiment not found."))
      }
      sample_info <- glyexp::get_sample_info(exp)
      group_col <- ctx$group_col
      if (!group_col %in% colnames(sample_info)) {
        return(list(
          check = FALSE,
          reason = paste0("Group column '", group_col, "' not found.")
        ))
      }
      n_groups <- nlevels(sample_info[[group_col]])
      if (n_groups != 2) {
        groups <- levels(sample_info[[group_col]])
        list(
          check = FALSE,
          reason = paste0(
            "OPLS-DA requires exactly 2 groups, but found ",
            n_groups,
            " groups (",
            paste(groups, collapse = ", "),
            ")."
          )
        )
      } else {
        list(check = TRUE, reason = NULL)
      }
    }
  )
}
