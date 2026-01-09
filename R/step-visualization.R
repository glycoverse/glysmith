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
