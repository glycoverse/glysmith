#' Step: Heatmap
#'
#' Create a heatmap plot using `glyvis::plot_heatmap()`.
#' The heatmap visualizes expression values across samples.
#' The upstream step supplies the experiment.
#'
#' @param plot_width Width of the plot in inches. Default is 7.
#' @param plot_height Height of the plot in inches. Default is 7.
#' @param ... Additional arguments passed to [glyvis::plot_heatmap()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_heatmap()
#' @seealso [glyvis::plot_heatmap()]
#' @export
step_heatmap <- function(plot_width = 7, plot_height = 7, ...) {
  .check_step_dots(sys.call())
  signature <- rlang::expr_deparse(match.call())

  plot_name <- "heatmap"
  label <- "Heatmap"

  step(
    id = "heatmap",
    connected = .exp_port(),
    label = label,
    run = function(ctx) {
      .run_heatmap(
        ctx,
        plot_name = plot_name,
        plot_width = plot_width,
        plot_height = plot_height,
        ...
      )
    },
    packages = c("glyvis", "pheatmap"),
    signature = signature
  )
}

#' Run heatmap plotting
#'
#' @param ctx Analysis context.
#' @param plot_name Plot identifier.
#' @param plot_width Plot width in inches.
#' @param plot_height Plot height in inches.
#' @param ... Additional arguments passed to [glyvis::plot_heatmap()].
#'
#' @returns Updated analysis context.
#' @noRd
.run_heatmap <- function(ctx, plot_name, plot_width, plot_height, ...) {
  exp <- ctx$data$exp
  p <- glyvis::plot_heatmap(exp, ...)
  ctx_add_plot(
    ctx,
    plot_name,
    p,
    paste0("Heatmap of the input experiment."),
    width = plot_width,
    height = plot_height
  )
}

#' Step: Plot the glycosite profile
#'
#' Plot two count distributions for the current glycoproteomics experiment
#' (`exp`), pooling annotations across samples. Glycomics inputs skip this step.
#'
#' @details
#' - `glycans_per_site` counts sites by the number of distinct glycans observed
#'   at each protein and site position, using [glyvis::plot_glycans_per_site()].
#' - `sites_per_protein` counts proteins by the number of distinct glycosylation
#'   sites observed on each protein, using [glyvis::plot_sites_per_protein()].
#'
#' @inheritParams glyvis::plot_glycans_per_site
#' @param plot_width,plot_height Export width and height of each plot in inches.
#' @return A `glysmith_step` object.
#' @examples
#' bp <- blueprint(step_plot_glycosite_profile())
#' if (requireNamespace("glyvis", quietly = TRUE)) {
#'   result <- forge_analysis(glyexp::real_experiment, bp)
#'   cast_plot(result, "node_1__glycans_per_site")
#'   cast_plot(result, "node_1__sites_per_protein")
#' }
#' @export
step_plot_glycosite_profile <- function(
  glycan_col = NULL,
  plot_width = 4,
  plot_height = 4
) {
  step(
    id = "plot_glycosite_profile",
    connected = .exp_port(),
    label = "Glycosite profile",
    packages = "glyvis",
    condition = function(ctx) {
      list(
        check = inherits(ctx_get_data(ctx, "exp"), "GlycoproteomicSE"),
        reason = "glycosite profiles require a glycoproteomics experiment"
      )
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      ctx |>
        ctx_add_plot(
          "glycans_per_site",
          glyvis::plot_glycans_per_site(exp, glycan_col = glycan_col),
          "Number of sites carrying each observed number of distinct glycans.",
          width = plot_width,
          height = plot_height
        ) |>
        ctx_add_plot(
          "sites_per_protein",
          glyvis::plot_sites_per_protein(exp),
          "Number of proteins carrying each observed number of glycosylation sites.",
          width = plot_width,
          height = plot_height
        )
    },
    signature = rlang::expr_deparse(match.call())
  )
}

#' Step: Logo Plot
#'
#' Create a logo plot for glycosylation sites using `glyvis::plot_logo()`.
#' The logo plot visualizes the amino acid sequence patterns around glycosylation sites.
#' This step is only applicable for glycoproteomics experiments.
#' The upstream step supplies the experiment.
#'
#' @param plot_width Width of the plot in inches. Default is 5.
#' @param plot_height Height of the plot in inches. Default is 3.
#' @inheritParams glyvis::plot_logo
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_logo()
#' step_logo(fasta = "proteins.fasta")
#' @seealso [glyvis::plot_logo()]
#' @export
step_logo <- function(
  n_aa = 5L,
  fasta = NULL,
  plot_width = 5,
  plot_height = 3,
  ...
) {
  .check_step_dots(sys.call())
  signature <- rlang::expr_deparse(match.call())

  plot_name <- "logo"
  label <- "Logo plot"

  step(
    id = "logo",
    connected = .exp_port(),
    label = label,
    condition = function(ctx) {
      .condition_logo(ctx)
    },
    run = function(ctx) {
      .run_logo(
        ctx,
        plot_name = plot_name,
        n_aa = n_aa,
        fasta = fasta,
        plot_width = plot_width,
        plot_height = plot_height,
        ...
      )
    },
    packages = c("glyvis", "ggseqlogo", "UniProt.ws"),
    signature = signature
  )
}

#' Check whether logo plotting should run
#'
#' @param ctx Analysis context.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_logo <- function(ctx) {
  exp <- ctx$data$exp
  if (.get_exp_type(exp) != "glycoproteomics") {
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
#' @param plot_name Plot identifier.
#' @inheritParams step_logo
#'
#' @returns Updated analysis context.
#' @noRd
.run_logo <- function(
  ctx,
  plot_name,
  n_aa,
  fasta,
  plot_width,
  plot_height,
  ...
) {
  exp <- ctx$data$exp
  p <- glyvis::plot_logo(exp, n_aa = n_aa, fasta = fasta, ...)
  ctx_add_plot(
    ctx,
    plot_name,
    p,
    paste0("Logo plot of glycosylation sites for the input experiment."),
    width = plot_width,
    height = plot_height
  )
}

#' Step: Significant Variables Boxplot
#'
#' Create boxplots for the most significant variables from DEA analysis using
#' `glyvis::plot_boxplot()`. The function selects the top `n_top` variables with
#' the lowest adjusted p-values from the DEA results and plots their expression
#' values grouped by sample groups.
#' The upstream step supplies the experiment.
#'
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
#' @seealso [glyvis::plot_boxplot()]
#' @export
step_sig_boxplot <- function(
  n_top = 25,
  panel_width = 1.5,
  panel_height = 1.2,
  min_width = 5,
  min_height = 3,
  max_width = 14,
  max_height = 12,
  ...
) {
  .check_step_dots(sys.call())
  checkmate::assert_int(n_top, lower = 1L, upper = 25L)
  checkmate::assert_number(panel_width, lower = 0.5)
  checkmate::assert_number(panel_height, lower = 0.5)
  checkmate::assert_number(min_width, lower = 1)
  checkmate::assert_number(min_height, lower = 1)
  checkmate::assert_number(max_width, lower = 1)
  checkmate::assert_number(max_height, lower = 1)
  signature <- rlang::expr_deparse(match.call())

  plot_name <- "sig_boxplot"
  label <- "Significant variables boxplot"

  step(
    id = "sig_boxplot",
    connected = list(accepts = "filtered_exp"),
    label = label,
    run = function(ctx) {
      .run_sig_boxplot(
        ctx,
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
    packages = c("glystats", "glyvis"),
    signature = signature
  )
}

#' Run significant-variable boxplotting
#'
#' @param ctx Analysis context.
#' @param plot_name Plot identifier.
#' @inheritParams step_sig_boxplot
#'
#' @returns Updated analysis context.
#' @noRd
.run_sig_boxplot <- function(
  ctx,
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
  exp <- ctx$data$exp
  n_vars <- nrow(exp)

  if (nrow(exp) > 25) {
    dea_res <- ctx$data$dea_res
    tidy_res <- .get_sig_boxplot_tidy_result(dea_res)

    id_col <- .stats_id_column(dea_res)
    if (isTRUE(dea_res$identifiers$variable$generated)) {
      cli::cli_abort(
        "Ranking a filtered boxplot requires unique fitted variable names."
      )
    }
    tidy_res <- dplyr::filter(tidy_res, .data[[id_col]] %in% rownames(exp))
    top_vars <- tidy_res |>
      dplyr::arrange(dplyr::across(dplyr::matches("p_adj"))) |>
      dplyr::distinct(.data[[id_col]], .keep_all = TRUE) |>
      dplyr::slice_head(n = min(n_top, 25)) |>
      dplyr::pull(dplyr::all_of(id_col))

    exp <- .filter_variables(exp, top_vars)
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
    paste0("Boxplot of significant variables from the input experiment."),
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
