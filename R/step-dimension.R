#' Step: Principal Component Analysis (PCA)
#'
#' Run PCA using `glystats::gly_pca()` and plot it with `glyvis::plot_pca()`.
#' Loading plot for glycoproteomics data can be crowded with too many variables.
#' Ignore the resulting plot if it is not informative.
#' The upstream step supplies the experiment.
#'
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
  center = TRUE,
  scale = TRUE,
  loadings = FALSE,
  screeplot = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
) {
  .check_step_dots(sys.call())
  signature <- rlang::expr_deparse(match.call())
  pca_args <- rlang::list2(...)
  id <- "pca"
  step(
    id = id,
    connected = .exp_port(),
    label = "Principal component analysis",
    run = function(ctx) {
      .run_pca(
        ctx,
        id = id,
        center = center,
        scale = scale,
        loadings = loadings,
        screeplot = screeplot,
        plot_width = plot_width,
        plot_height = plot_height,
        pca_args = pca_args
      )
    },
    packages = c("glystats", "glyvis"),
    signature = signature
  )
}

#' Run PCA
#'
#' @param ctx Analysis context.
#' @param id Step identifier.
#' @inheritParams step_pca
#' @param pca_args Additional arguments passed to `glystats::gly_pca()`.
#'
#' @returns Updated analysis context.
#' @noRd
.run_pca <- function(
  ctx,
  id,
  center,
  scale,
  loadings,
  screeplot,
  plot_width,
  plot_height,
  pca_args
) {
  exp <- ctx$data$exp
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
    paste0("PCA scores for each sample of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_variables"),
    glystats::get_tidy_result(pca_res, "variables"),
    paste0("PCA loadings for each variable of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_eigenvalues"),
    glystats::get_tidy_result(pca_res, "eigenvalues"),
    paste0("PCA eigenvalues of the input experiment.")
  )
  p_scores <- glyvis::plot_pca(pca_res, type = "individual")
  ctx <- ctx_add_plot(
    ctx,
    paste0(id, "_scores"),
    p_scores,
    paste0("PCA score plot colored by group of the input experiment."),
    width = plot_width,
    height = plot_height
  )
  if (loadings) {
    p_loadings <- glyvis::plot_pca(pca_res, type = "variables")
    ctx <- ctx_add_plot(
      ctx,
      paste0(id, "_loadings"),
      p_loadings,
      paste0("PCA loading plot of the input experiment."),
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
      paste0("PCA screeplot of the input experiment."),
      width = plot_width,
      height = plot_height
    )
  }
  ctx
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
#' The upstream step supplies the experiment.
#'
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
  dims = 2,
  perplexity = 30,
  plot_width = 5,
  plot_height = 5,
  ...
) {
  .check_step_dots(sys.call())
  signature <- rlang::expr_deparse(match.call())
  tsne_args <- rlang::list2(...)
  id <- "tsne"

  step(
    id = id,
    connected = .exp_port(),
    label = "t-SNE",
    run = function(ctx) {
      .run_tsne(
        ctx,
        id = id,
        dims = dims,
        perplexity = perplexity,
        plot_width = plot_width,
        plot_height = plot_height,
        tsne_args = tsne_args
      )
    },
    packages = c("glystats", "glyvis", "Rtsne"),
    signature = signature
  )
}

#' Run t-SNE
#'
#' @param ctx Analysis context.
#' @param id Step identifier.
#' @inheritParams step_tsne
#' @param tsne_args Additional arguments passed to `glystats::gly_tsne()`.
#'
#' @returns Updated analysis context.
#' @noRd
.run_tsne <- function(
  ctx,
  id,
  dims,
  perplexity,
  plot_width,
  plot_height,
  tsne_args
) {
  exp <- ctx$data$exp
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
    paste0("t-SNE result of the input experiment.")
  )
  p <- glyvis::plot_tsne(tsne)
  ctx_add_plot(
    ctx,
    id,
    p,
    paste0("t-SNE plot of the input experiment."),
    width = plot_width,
    height = plot_height
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
#' The upstream step supplies the experiment.
#'
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
  n_neighbors = 15,
  n_components = 2,
  plot_width = 5,
  plot_height = 5,
  ...
) {
  .check_step_dots(sys.call())
  signature <- rlang::expr_deparse(match.call())
  umap_args <- rlang::list2(...)
  id <- "umap"

  step(
    id = id,
    connected = .exp_port(),
    label = "UMAP",
    run = function(ctx) {
      .run_umap(
        ctx,
        id = id,
        n_neighbors = n_neighbors,
        n_components = n_components,
        plot_width = plot_width,
        plot_height = plot_height,
        umap_args = umap_args
      )
    },
    packages = c("glystats", "glyvis", "uwot"),
    signature = signature
  )
}

#' Run UMAP
#'
#' @param ctx Analysis context.
#' @param id Step identifier.
#' @inheritParams step_umap
#' @param umap_args Additional arguments passed to `glystats::gly_umap()`.
#'
#' @returns Updated analysis context.
#' @noRd
.run_umap <- function(
  ctx,
  id,
  n_neighbors,
  n_components,
  plot_width,
  plot_height,
  umap_args
) {
  exp <- ctx$data$exp
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
    paste0("UMAP result of the input experiment.")
  )
  p <- glyvis::plot_umap(umap)
  ctx_add_plot(
    ctx,
    id,
    p,
    paste0("UMAP plot of the input experiment."),
    width = plot_width,
    height = plot_height
  )
}

#' Step: Partial Least Squares Discriminant Analysis (PLS-DA)
#'
#' Perform PLS-DA using `glystats::gly_plsda()` and plot it with `glyvis::plot_plsda()`.
#' PLS-DA is a supervised method that finds components maximizing covariance between
#' predictors and the response variable (group membership).
#' The upstream step supplies the experiment.
#'
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
  ncomp = 2,
  scale = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
) {
  .check_step_dots(sys.call())
  signature <- rlang::expr_deparse(match.call())
  plsda_args <- rlang::list2(...)
  id <- "plsda"

  step(
    id = id,
    connected = .exp_port(),
    label = "Partial least squares discriminant analysis",
    run = function(ctx) {
      .run_plsda(
        ctx,
        id = id,
        ncomp = ncomp,
        scale = scale,
        plot_width = plot_width,
        plot_height = plot_height,
        plsda_args = plsda_args
      )
    },
    packages = c("glystats", "glyvis", "ropls"),
    signature = signature,
    report = function(x) {
      .report_plsda(x, id = id)
    }
  )
}

#' Run PLS-DA
#'
#' @param ctx Analysis context.
#' @param id Step identifier.
#' @inheritParams step_plsda
#' @param plsda_args Additional arguments passed to `glystats::gly_plsda()`.
#'
#' @returns Updated analysis context.
#' @noRd
.run_plsda <- function(
  ctx,
  id,
  ncomp,
  scale,
  plot_width,
  plot_height,
  plsda_args
) {
  exp <- ctx$data$exp
  plsda_res <- rlang::exec(
    glystats::gly_plsda,
    exp,
    ncomp = ncomp,
    scale = scale,
    !!!plsda_args
  )
  tidy_res <- plsda_res$tidy_result
  ctx <- ctx_add_data(
    ctx,
    paste0(id, "_id_column"),
    .stats_id_column(plsda_res)
  )

  ctx <- ctx_add_data(ctx, paste0(id, "_raw_res"), plsda_res$raw_result)
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_samples"),
    tidy_res$samples,
    paste0("PLS-DA scores for each sample of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_variables"),
    tidy_res$variables,
    paste0("PLS-DA loadings for each variable of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_variance"),
    tidy_res$variance,
    paste0("PLS-DA explained variance of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_vip"),
    tidy_res$vip,
    paste0("PLS-DA VIP scores of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_perm_test"),
    tidy_res$perm_test,
    paste0("PLS-DA permutation test results of the input experiment.")
  )

  p_scores <- glyvis::plot_plsda(plsda_res, type = "scores")
  ctx <- ctx_add_plot(
    ctx,
    paste0(id, "_scores"),
    p_scores,
    paste0("PLS-DA score plot colored by group of the input experiment."),
    width = plot_width,
    height = plot_height
  )
  p_loadings <- glyvis::plot_plsda(plsda_res, type = "loadings")
  ctx <- ctx_add_plot(
    ctx,
    paste0(id, "_loadings"),
    p_loadings,
    paste0("PLS-DA loading plot of the input experiment."),
    width = plot_width,
    height = plot_height
  )
  p_variance <- glyvis::plot_plsda(plsda_res, type = "variance")
  ctx <- ctx_add_plot(
    ctx,
    paste0(id, "_variance"),
    p_variance,
    paste0("PLS-DA variance plot of the input experiment."),
    width = plot_width,
    height = plot_height
  )
  p_vip <- glyvis::plot_plsda(plsda_res, type = "vip")
  ctx_add_plot(
    ctx,
    paste0(id, "_vip"),
    p_vip,
    paste0("PLS-DA VIP score plot of the input experiment."),
    width = plot_width,
    height = plot_height
  )
}

#' Report PLS-DA results
#'
#' @param x Analysis context after running PLS-DA.
#' @param id Step identifier.
#'
#' @returns A summary string for the report.
#' @noRd
.report_plsda <- function(x, id) {
  raw_res <- x$data[[paste0(id, "_raw_res")]]
  model_df <- raw_res@modelDF
  r2y <- model_df[["R2Y"]][1]
  r2y_cum <- model_df[["R2Y(cum)"]][1]
  q2 <- model_df[["Q2"]][1]
  q2_cum <- model_df[["Q2(cum)"]][1]

  vip_tbl <- x$tables[[paste0(id, "_vip")]]
  n_vip_gt_1 <- sum(vip_tbl$vip > 1, na.rm = TRUE)

  top5 <- vip_tbl |>
    dplyr::arrange(dplyr::desc(.data$vip)) |>
    dplyr::slice_head(n = 5)
  id_col <- x$data[[paste0(id, "_id_column")]] %||% "variable"
  top5_vars <- top5[[id_col]]

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

#' Step: Orthogonal Partial Least Squares Discriminant Analysis (OPLS-DA)
#'
#' Perform OPLS-DA using `glystats::gly_oplsda()` and plot it with `glyvis::plot_oplsda()`.
#' OPLS-DA separates variation into predictive (related to group) and orthogonal (unrelated) components.
#' This step only works with binary classification (exactly 2 groups).
#' The upstream step supplies the experiment.
#'
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
  pred_i = 1,
  ortho_i = NA,
  scale = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
) {
  .check_step_dots(sys.call())
  signature <- rlang::expr_deparse(match.call())
  oplsda_args <- rlang::list2(...)
  id <- "oplsda"

  step(
    id = id,
    connected = .exp_port(),
    label = "Orthogonal partial least squares discriminant analysis",
    run = function(ctx) {
      .run_oplsda(
        ctx,
        id = id,
        pred_i = pred_i,
        ortho_i = ortho_i,
        scale = scale,
        plot_width = plot_width,
        plot_height = plot_height,
        oplsda_args = oplsda_args
      )
    },
    packages = c("glystats", "glyvis", "ropls"),
    signature = signature,
    report = function(x) {
      .report_oplsda(x, id = id)
    },
    condition = function(ctx) {
      .condition_oplsda(ctx)
    }
  )
}

#' Run OPLS-DA
#'
#' @param ctx Analysis context.
#' @param id Step identifier.
#' @inheritParams step_oplsda
#' @param oplsda_args Additional arguments passed to `glystats::gly_oplsda()`.
#'
#' @returns Updated analysis context.
#' @noRd
.run_oplsda <- function(
  ctx,
  id,
  pred_i,
  ortho_i,
  scale,
  plot_width,
  plot_height,
  oplsda_args
) {
  exp <- ctx$data$exp
  oplsda_res <- rlang::exec(
    glystats::gly_oplsda,
    exp,
    pred_i = pred_i,
    ortho_i = ortho_i,
    scale = scale,
    !!!oplsda_args
  )
  tidy_res <- oplsda_res$tidy_result
  ctx <- ctx_add_data(
    ctx,
    paste0(id, "_id_column"),
    .stats_id_column(oplsda_res)
  )

  ctx <- ctx_add_data(ctx, paste0(id, "_raw_res"), oplsda_res$raw_result)
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_samples"),
    tidy_res$samples,
    paste0("OPLS-DA scores for each sample of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_variables"),
    tidy_res$variables,
    paste0("OPLS-DA loadings for each variable of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_variance"),
    tidy_res$variance,
    paste0("OPLS-DA explained variance of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_vip"),
    tidy_res$vip,
    paste0("OPLS-DA VIP scores of the input experiment.")
  )
  ctx <- ctx_add_table(
    ctx,
    paste0(id, "_perm_test"),
    tidy_res$perm_test,
    paste0("OPLS-DA permutation test results of the input experiment.")
  )

  p_scores <- glyvis::plot_oplsda(oplsda_res, type = "scores")
  ctx <- ctx_add_plot(
    ctx,
    paste0(id, "_scores"),
    p_scores,
    paste0("OPLS-DA score plot colored by group of the input experiment."),
    width = plot_width,
    height = plot_height
  )
  p_loadings <- glyvis::plot_oplsda(oplsda_res, type = "loadings")
  ctx <- ctx_add_plot(
    ctx,
    paste0(id, "_loadings"),
    p_loadings,
    paste0("OPLS-DA loading plot of the input experiment."),
    width = plot_width,
    height = plot_height
  )
  p_variance <- glyvis::plot_oplsda(oplsda_res, type = "variance")
  ctx <- ctx_add_plot(
    ctx,
    paste0(id, "_variance"),
    p_variance,
    paste0("OPLS-DA variance plot of the input experiment."),
    width = plot_width,
    height = plot_height
  )
  p_vip <- glyvis::plot_oplsda(oplsda_res, type = "vip")
  ctx_add_plot(
    ctx,
    paste0(id, "_vip"),
    p_vip,
    paste0("OPLS-DA VIP score plot of the input experiment."),
    width = plot_width,
    height = plot_height
  )
}

#' Report OPLS-DA results
#'
#' @param x Analysis context after running OPLS-DA.
#' @param id Step identifier.
#'
#' @returns A summary string for the report.
#' @noRd
.report_oplsda <- function(x, id) {
  raw_res <- x$data[[paste0(id, "_raw_res")]]
  model_df <- raw_res@modelDF
  r2y <- model_df[["R2Y"]][1]
  r2y_cum <- model_df[["R2Y(cum)"]][1]
  q2 <- model_df[["Q2"]][1]
  q2_cum <- model_df[["Q2(cum)"]][1]

  vip_tbl <- x$tables[[paste0(id, "_vip")]]
  n_vip_gt_1 <- sum(vip_tbl$vip > 1, na.rm = TRUE)

  top5 <- vip_tbl |>
    dplyr::arrange(dplyr::desc(.data$vip)) |>
    dplyr::slice_head(n = 5)
  id_col <- x$data[[paste0(id, "_id_column")]] %||% "variable"
  top5_vars <- top5[[id_col]]

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
}

#' Check whether OPLS-DA should run
#'
#' @param ctx Analysis context.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_oplsda <- function(ctx) {
  exp <- tryCatch(ctx$data$exp, error = function(e) NULL)
  if (is.null(exp)) {
    return(list(check = FALSE, reason = "Experiment not found."))
  }
  sample_info <- .get_sample_info(exp)
  group_col <- ctx$group_col
  if (!group_col %in% colnames(sample_info)) {
    return(list(
      check = FALSE,
      reason = paste0("Group column '", group_col, "' not found.")
    ))
  }
  group <- droplevels(as.factor(sample_info[[group_col]]))
  n_groups <- nlevels(group)
  if (n_groups != 2) {
    groups <- levels(group)
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
