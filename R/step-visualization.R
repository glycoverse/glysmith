#' Step: Heatmap
#'
#' @description
#' Create a heatmap plot using `glyvis::plot_heatmap()`.
#' The heatmap visualizes expression values across samples.
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
#' - Depends on `on` parameter (default: `exp`)
#'
#' Plots generated:
#' - `heatmap`: A heatmap plot (if `on = "exp"`)
#' - `sig_heatmap`: A heatmap plot (if `on = "sig_exp"`)
#' - `trait_heatmap`: A heatmap plot (if `on = "trait_exp"`)
#' - `sig_trait_heatmap`: A heatmap plot (if `on = "sig_trait_exp"`)
#' - `dynamic_motif_heatmap`: A heatmap plot (if `on = "dynamic_motif_exp"`)
#' - `sig_dynamic_motif_heatmap`: A heatmap plot (if `on = "sig_dynamic_motif_exp"`)
#' - `branch_motif_heatmap`: A heatmap plot (if `on = "branch_motif_exp"`)
#' - `sig_branch_motif_heatmap`: A heatmap plot (if `on = "sig_branch_motif_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "exp", "sig_exp", "trait_exp", "sig_trait_exp",
#'   "dynamic_motif_exp", "sig_dynamic_motif_exp", "branch_motif_exp", "sig_branch_motif_exp".
#'   Default is "exp".
#' @param plot_width Width of the plot in inches. Default is 7.
#' @param plot_height Height of the plot in inches. Default is 7.
#' @param ... Additional arguments passed to [glyvis::plot_heatmap()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_heatmap()
#' step_heatmap(on = "sig_exp")
#' step_heatmap(on = "trait_exp")
#' @seealso [glyvis::plot_heatmap()]
#' @export
step_heatmap <- function(on = "exp", plot_width = 7, plot_height = 7, ...) {
  signature <- rlang::expr_deparse(match.call())

  on_meta <- .resolve_on(on)
  plot_name <- paste0("heatmap", on_meta$id_suffix)
  label <- paste0("Heatmap", on_meta$label_suffix)

  step(
    id = paste0("heatmap", on_meta$id_suffix),
    label = label,
    run = function(ctx) {
      .run_heatmap(
        ctx,
        on = on,
        plot_name = plot_name,
        plot_width = plot_width,
        plot_height = plot_height,
        ...
      )
    },
    require = on,
    packages = c("glyvis", "pheatmap"),
    signature = signature
  )
}

#' Run heatmap plotting
#'
#' @param ctx Analysis context.
#' @param on Name of the experiment data in `ctx$data`.
#' @param plot_name Plot identifier.
#' @param plot_width Plot width in inches.
#' @param plot_height Plot height in inches.
#' @param ... Additional arguments passed to [glyvis::plot_heatmap()].
#'
#' @returns Updated analysis context.
#' @noRd
.run_heatmap <- function(ctx, on, plot_name, plot_width, plot_height, ...) {
  exp <- ctx_get_data(ctx, on)
  p <- glyvis::plot_heatmap(exp, ...)
  ctx_add_plot(
    ctx,
    plot_name,
    p,
    paste0("Heatmap of ", on, "."),
    width = plot_width,
    height = plot_height
  )
}

#' Step: Logo Plot
#'
#' @description
#' Create a logo plot for glycosylation sites using `glyvis::plot_logo()`.
#' The logo plot visualizes the amino acid sequence patterns around glycosylation sites.
#' This step is only applicable for glycoproteomics experiments.
#'
#' This step depends on the `on` parameter (default: `exp`).
#' - When `on = "exp"`, requires `exp` (experiment data).
#' - When `on = "sig_exp"`, requires `sig_exp` from one of [step_dea_limma()],
#'   [step_dea_ttest()], [step_dea_wilcox()], [step_dea_anova()], or [step_dea_kruskal()].
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Plots generated:
#' - `logo`: A logo plot (if `on = "exp"`)
#' - `sig_logo`: A logo plot (if `on = "sig_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "exp", "sig_exp". Default is "exp".
#' @param plot_width Width of the plot in inches. Default is 5.
#' @param plot_height Height of the plot in inches. Default is 3.
#' @inheritParams glyvis::plot_logo
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_logo()
#' step_logo(fasta = "proteins.fasta")
#' step_logo(on = "sig_exp")
#' @seealso [glyvis::plot_logo()]
#' @export
step_logo <- function(
  on = "exp",
  n_aa = 5L,
  fasta = NULL,
  plot_width = 5,
  plot_height = 3,
  ...
) {
  checkmate::assert_choice(on, c("exp", "sig_exp"))
  signature <- rlang::expr_deparse(match.call())

  on_meta <- .resolve_on(on)
  plot_name <- paste0("logo", on_meta$id_suffix)
  label <- paste0("Logo plot", on_meta$label_suffix)

  step(
    id = paste0("logo", on_meta$id_suffix),
    label = label,
    condition = function(ctx) {
      .condition_logo(ctx, on = on)
    },
    run = function(ctx) {
      .run_logo(
        ctx,
        on = on,
        plot_name = plot_name,
        n_aa = n_aa,
        fasta = fasta,
        plot_width = plot_width,
        plot_height = plot_height,
        ...
      )
    },
    require = on,
    packages = c("glyvis", "ggseqlogo", "UniProt.ws"),
    signature = signature
  )
}

#' Check whether logo plotting should run
#'
#' @param ctx Analysis context.
#' @param on Name of the experiment data in `ctx$data`.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_logo <- function(ctx, on) {
  exp <- ctx_get_data(ctx, on)
  if (glyexp::get_exp_type(exp) != "glycoproteomics") {
    return(list(
      check = FALSE,
      reason = "logo plot is only applicable for glycoproteomics experiments"
    ))
  }
  list(check = TRUE, reason = NULL)
}

#' Run logo plotting
#'
#' @param ctx Analysis context.
#' @param on Name of the experiment data in `ctx$data`.
#' @param plot_name Plot identifier.
#' @inheritParams step_logo
#'
#' @returns Updated analysis context.
#' @noRd
.run_logo <- function(
  ctx,
  on,
  plot_name,
  n_aa,
  fasta,
  plot_width,
  plot_height,
  ...
) {
  exp <- ctx_get_data(ctx, on)
  p <- glyvis::plot_logo(exp, n_aa = n_aa, fasta = fasta, ...)
  ctx_add_plot(
    ctx,
    plot_name,
    p,
    paste0("Logo plot of glycosylation sites for ", on, "."),
    width = plot_width,
    height = plot_height
  )
}

#' Step: Significant Variables Boxplot
#'
#' @description
#' Create boxplots for the most significant variables from DEA analysis using
#' `glyvis::plot_boxplot()`. The function selects the top `n_top` variables with
#' the lowest adjusted p-values from the DEA results and plots their expression
#' values grouped by sample groups.
#'
#' This step depends on the `on` parameter (default: `sig_exp`).
#' - When `on = "sig_exp"`, requires `sig_exp` from one of [step_dea_limma()],
#'   [step_dea_ttest()], [step_dea_wilcox()], [step_dea_anova()], or [step_dea_kruskal()].
#' - When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on traits.
#' - When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp` from DEA on motifs.
#' - When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp` from DEA on motifs.
#'
#' The number of variables is limited to a maximum of 25, as enforced by
#' `glyvis::plot_boxplot()`.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter:
#'   - `sig_exp` (default): Significant experiment from DEA
#'   - `sig_trait_exp`: Significant trait experiment from DTA
#'   - `sig_dynamic_motif_exp`: Significant dynamic motif experiment from DMA
#'   - `sig_branch_motif_exp`: Significant branch motif experiment from DMA
#'
#' Plots generated:
#' - `sig_boxplot`: A boxplot of significant variables (if `on = "sig_exp"`)
#' - `sig_trait_boxplot`: A boxplot of significant traits (if `on = "sig_trait_exp"`)
#' - `sig_dynamic_motif_boxplot`: A boxplot of significant dynamic motifs (if `on = "sig_dynamic_motif_exp"`)
#' - `sig_branch_motif_boxplot`: A boxplot of significant branch motifs (if `on = "sig_branch_motif_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "sig_exp", "sig_trait_exp", "sig_dynamic_motif_exp", "sig_branch_motif_exp".
#'   Default is "sig_exp".
#' @param n_top Number of top significant variables to plot.
#'   Must be between 1 and 25 (inclusive).
#'   Default is 25.
#' @param panel_width Width of each panel in inches. Default is 1.5.
#' @param panel_height Height of each panel in inches. Default is 1.2.
#' @param min_width Minimum plot width in inches. Default is 5.
#' @param min_height Minimum plot height in inches. Default is 3.
#' @param max_width Maximum plot width in inches. Default is 14.
#' @param max_height Maximum plot height in inches. Default is 12.
#' @param ... Additional arguments passed to [glyvis::plot_boxplot()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_sig_boxplot()
#' step_sig_boxplot(n_top = 12)
#' step_sig_boxplot(on = "sig_trait_exp")
#' @seealso [glyvis::plot_boxplot()]
#' @export
step_sig_boxplot <- function(
  on = "sig_exp",
  n_top = 25,
  panel_width = 1.5,
  panel_height = 1.2,
  min_width = 5,
  min_height = 3,
  max_width = 14,
  max_height = 12,
  ...
) {
  checkmate::assert_choice(
    on,
    c(
      "sig_exp",
      "sig_trait_exp",
      "sig_dynamic_motif_exp",
      "sig_branch_motif_exp"
    )
  )
  checkmate::assert_int(n_top, lower = 1L, upper = 25L)
  checkmate::assert_number(panel_width, lower = 0.5)
  checkmate::assert_number(panel_height, lower = 0.5)
  checkmate::assert_number(min_width, lower = 1)
  checkmate::assert_number(min_height, lower = 1)
  checkmate::assert_number(max_width, lower = 1)
  checkmate::assert_number(max_height, lower = 1)
  signature <- rlang::expr_deparse(match.call())

  on_meta <- .resolve_on(on)
  plot_name <- paste0("sig_boxplot", on_meta$id_suffix)
  label <- paste0("Significant variables boxplot", on_meta$label_suffix)

  step(
    id = paste0("sig_boxplot", on_meta$id_suffix),
    label = label,
    run = function(ctx) {
      .run_sig_boxplot(
        ctx,
        on = on,
        plot_name = plot_name,
        n_top = n_top,
        panel_width = panel_width,
        panel_height = panel_height,
        min_width = min_width,
        min_height = min_height,
        max_width = max_width,
        max_height = max_height,
        ...
      )
    },
    require = on,
    packages = c("glystats", "glyvis"),
    signature = signature
  )
}

#' Run significant-variable boxplotting
#'
#' @param ctx Analysis context.
#' @param on Name of the experiment data in `ctx$data`.
#' @param plot_name Plot identifier.
#' @inheritParams step_sig_boxplot
#'
#' @returns Updated analysis context.
#' @noRd
.run_sig_boxplot <- function(
  ctx,
  on,
  plot_name,
  n_top,
  panel_width,
  panel_height,
  min_width,
  min_height,
  max_width,
  max_height,
  ...
) {
  exp <- ctx_get_data(ctx, on)
  n_vars <- nrow(exp)

  if (nrow(exp) > 25) {
    dea_key <- switch(
      on,
      sig_exp = "dea_res",
      sig_trait_exp = "dta_res",
      sig_dynamic_motif_exp = "dynamic_dma_res",
      sig_branch_motif_exp = "branch_dma_res"
    )
    dea_res <- ctx_get_data(ctx, dea_key)
    tidy_res <- .get_sig_boxplot_tidy_result(dea_res)

    top_vars <- tidy_res |>
      dplyr::arrange(dplyr::across(dplyr::matches("p_adj"))) |>
      dplyr::distinct(.data$variable, .keep_all = TRUE) |>
      dplyr::slice_head(n = min(n_top, 25)) |>
      dplyr::pull(.data$variable)

    exp <- exp |>
      glyexp::filter_var(.data$variable %in% top_vars)
    n_vars <- length(top_vars)
  }

  p <- glyvis::plot_boxplot(exp, group_col = ctx$group_col, ...)

  dims <- .calc_boxplot_dims(
    n_vars,
    panel_width,
    panel_height,
    min_width,
    min_height,
    max_width,
    max_height
  )

  ctx_add_plot(
    ctx,
    plot_name,
    p,
    paste0("Boxplot of significant variables from ", on, "."),
    width = dims$width,
    height = dims$height
  )
}

#' Calculate dynamic plot dimensions for boxplot based on number of panels
#'
#' @param n_vars Number of variables (panels) to plot.
#' @param panel_width Width of each panel in inches.
#' @param panel_height Height of each panel in inches.
#' @param min_width Minimum plot width in inches.
#' @param min_height Minimum plot height in inches.
#' @param max_width Maximum plot width in inches.
#' @param max_height Maximum plot height in inches.
#'
#' @return List with `width` and `height` elements.
#' @noRd
.calc_boxplot_dims <- function(
  n_vars,
  panel_width,
  panel_height,
  min_width,
  min_height,
  max_width,
  max_height
) {
  # Calculate optimal ncol to make the plot roughly square per panel
  ncol <- ceiling(sqrt(n_vars))
  nrow <- ceiling(n_vars / ncol)

  # Calculate total dimensions
  width <- ncol * panel_width + 1.5 # Add margin for axis labels
  height <- nrow * panel_height + 2 # Add margin for title and legend

  # Apply constraints
  width <- max(min_width, min(width, max_width))
  height <- max(min_height, min(height, max_height))

  list(width = width, height = height)
}

#' Get DEA result table used to rank significant boxplot variables
#'
#' @param dea_res A glystats differential-analysis result object.
#'
#' @returns A tidy result table.
#' @noRd
.get_sig_boxplot_tidy_result <- function(dea_res) {
  if (inherits(dea_res, c("glystats_anova_res", "glystats_kruskal_res"))) {
    return(glystats::get_tidy_result(dea_res, which = "main_test"))
  }

  glystats::get_tidy_result(dea_res)
}
