#' Step: Heatmap
#'
#' Create a heatmap plot using `glyvis::plot_heatmap()`.
#' The heatmap visualizes expression values across samples.
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
#' - `motif_heatmap`: A heatmap plot (if `on = "motif_exp"`)
#' - `sig_motif_heatmap`: A heatmap plot (if `on = "sig_motif_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#' - It is recommended to use this step on significant results (e.g. `on = "sig_exp"`) if available.
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
#'   Default is "exp".
#' @param ... Additional arguments passed to [glyvis::plot_heatmap()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_heatmap()
#' step_heatmap(on = "sig_exp")
#' step_heatmap(on = "trait_exp")
#' @seealso [glyvis::plot_heatmap()]
#' @export
step_heatmap <- function(on = "exp", ...) {
  rlang::check_installed("pheatmap")
  rlang::check_installed("ggplotify")
  signature <- rlang::expr_deparse(match.call())

  on_meta <- .resolve_on(on)
  plot_name <- paste0("heatmap", on_meta$id_suffix)
  label <- paste0("Heatmap", on_meta$label_suffix)

  step(
    id = paste0("heatmap", on_meta$id_suffix),
    label = label,
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      p <- glyvis::plot_heatmap(exp, ...)
      ctx_add_plot(ctx, plot_name, p, paste0("Heatmap of ", on, "."))
    },
    require = on,
    signature = signature
  )
}

#' Step: Logo Plot
#'
#' Create a logo plot for glycosylation sites using `glyvis::plot_logo()`.
#' The logo plot visualizes the amino acid sequence patterns around glycosylation sites.
#' This step is only applicable for glycoproteomics experiments.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Plots generated:
#' - `logo`: A logo plot (if `on = "exp"`)
#' - `sig_logo`: A logo plot (if `on = "sig_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if the user explicitly asks for logo plot.
#' - If used, ask user if a FASTA file is provided.
#'   Tell the user that if not, protein sequences will be fetched from Uniprot automatically.
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "exp", "sig_exp". Default is "exp".
#' @inheritParams glyvis::plot_logo
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_logo()
#' step_logo(fasta = "proteins.fasta")
#' step_logo(on = "sig_exp")
#' @seealso [glyvis::plot_logo()]
#' @export
step_logo <- function(on = "exp", n_aa = 5L, fasta = NULL, ...) {
  rlang::check_installed("ggseqlogo")
  checkmate::assert_choice(on, c("exp", "sig_exp"))
  signature <- rlang::expr_deparse(match.call())

  on_meta <- .resolve_on(on)
  plot_name <- paste0("logo", on_meta$id_suffix)
  label <- paste0("Logo plot", on_meta$label_suffix)

  step(
    id = paste0("logo", on_meta$id_suffix),
    label = label,
    condition = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      if (glyexp::get_exp_type(exp) != "glycoproteomics") {
        return(list(check = FALSE, reason = "logo plot is only applicable for glycoproteomics experiments"))
      }
      list(check = TRUE, reason = NULL)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      p <- glyvis::plot_logo(exp, n_aa = n_aa, fasta = fasta, ...)
      ctx_add_plot(ctx, plot_name, p, paste0("Logo plot of glycosylation sites for ", on, "."))
    },
    require = on,
    signature = signature
  )
}

#' Step: Significant Variables Boxplot
#'
#' Create boxplots for the most significant variables from DEA analysis using
#' `glyvis::plot_boxplot()`. The function selects the top `n_top` variables with
#' the lowest adjusted p-values from the DEA results and plots their expression
#' values grouped by sample groups.
#'
#' This step requires a DEA step to be run first (e.g., [step_dea_limma()],
#' [step_dea_ttest()], [step_dea_wilcox()], [step_dea_anova()], or [step_dea_kruskal()]).
#' The number of variables is limited to a maximum of 25, as enforced by
#' `glyvis::plot_boxplot()`.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter:
#'   - `sig_exp` (default): Significant experiment from DEA
#'   - `sig_trait_exp`: Significant trait experiment from DTA
#'   - `sig_motif_exp`: Significant motif experiment from DMA
#'
#' Plots generated:
#' - `sig_boxplot`: A boxplot of significant variables (if `on = "sig_exp"`)
#' - `sig_trait_boxplot`: A boxplot of significant traits (if `on = "sig_trait_exp"`)
#' - `sig_motif_boxplot`: A boxplot of significant motifs (if `on = "sig_motif_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step after DEA steps to visualize the significant variables.
#' - This step is particularly useful for understanding the expression patterns
#'   of the most differentially expressed features across groups.
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "sig_exp", "sig_trait_exp", "sig_motif_exp".
#'   Default is "sig_exp".
#' @param n_top Number of top significant variables to plot.
#'   Must be between 1 and 25 (inclusive).
#'   Default is 25.
#' @param ... Additional arguments passed to [glyvis::plot_boxplot()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_sig_boxplot()
#' step_sig_boxplot(n_top = 12)
#' step_sig_boxplot(on = "sig_trait_exp")
#' @seealso [glyvis::plot_boxplot()]
#' @export
step_sig_boxplot <- function(on = "sig_exp", n_top = 25, ...) {
  checkmate::assert_choice(on, c("sig_exp", "sig_trait_exp", "sig_motif_exp"))
  checkmate::assert_int(n_top, lower = 1L, upper = 25L)
  signature <- rlang::expr_deparse(match.call())

  on_meta <- .resolve_on(on)
  plot_name <- paste0("sig_boxplot", on_meta$id_suffix)
  label <- paste0("Significant variables boxplot", on_meta$label_suffix)

  step(
    id = paste0("sig_boxplot", on_meta$id_suffix),
    label = label,
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)

      # glyvis::plot_boxplot has a limit of 25 variables
      # If experiment has more than 25 variables, select top n_top by p-value
      if (nrow(exp) > 25) {
        # Get DEA results to select top by p-value
        dea_key <- switch(on,
          sig_exp = "dea_res",
          sig_trait_exp = "dta_res",
          sig_motif_exp = "dma_res"
        )
        dea_res <- ctx_get_data(ctx, dea_key)
        tidy_res <- glystats::get_tidy_result(dea_res)

        # Select top n_top variables by adjusted p-value (unique)
        top_vars <- tidy_res |>
          dplyr::arrange(dplyr::across(dplyr::matches("p_adj"))) |>
          dplyr::distinct(.data$variable, .keep_all = TRUE) |>
          dplyr::slice_head(n = min(n_top, 25)) |>
          dplyr::pull(.data$variable)

        exp <- exp |>
          glyexp::filter_var(.data$variable %in% top_vars)
      }

      p <- glyvis::plot_boxplot(exp, group_col = ctx$group_col, ...)
      ctx_add_plot(ctx, plot_name, p, paste0("Boxplot of significant variables from ", on, "."))
    },
    require = on,
    signature = signature
  )
}
