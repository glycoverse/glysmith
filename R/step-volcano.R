#' Step: Volcano Plot
#'
#' Create a volcano plot from DEA results using `glyvis::plot_volcano()`.
#' This step requires one of the DEA steps to be run:
#' - [step_dea_limma()] (multi-group comparison is also supported)
#' - [step_dea_ttest()]
#' - [step_dea_wilcox()]
#'
#' @details
#' Data required:
#' - `dea_res`: The DEA results from `glystats::gly_limma()`
#'
#' Plots generated:
#' - `volcano`: A volcano plot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Always include this step by default if DEA is performed, and the DEA method is not ANOVA or Kruskal-Wallis.
#'
#' @inheritParams glyvis::plot_volcano
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_volcano()
#' step_volcano(log2fc_cutoff = 2)
#' @seealso [glyvis::plot_volcano()]
#' @export
step_volcano <- function(log2fc_cutoff = 1, p_cutoff = 0.05, p_col = "p_adj", ...) {
  rlang::check_installed("EnhancedVolcano")
  signature <- rlang::expr_deparse(match.call())
  step(
    id = "volcano",
    label = "Volcano plot",
    condition = function(ctx) {
      dea_res <- ctx_get_data(ctx, "dea_res")
      check <- !inherits(dea_res, "glystats_anova_res") &&
        !inherits(dea_res, "glystats_kruskal_res")
      reason <- "volcano plot is not supported for ANOVA or Kruskal-Wallis DEA results."
      list(check = check, reason = reason)
    },
    run = function(ctx) {
      dea_res <- ctx_get_data(ctx, "dea_res")
      if (inherits(dea_res, "glystats_limma_res")) {
        .run_step_volcano_limma(ctx, log2fc_cutoff, p_cutoff, p_col, ...)
      } else {
        .run_step_volcano_ttest_wilcox(ctx, log2fc_cutoff, p_cutoff, p_col, ...)
      }
    },
    require = "dea_res",
    signature = signature
  )
}

.run_step_volcano_limma <- function(ctx, log2fc_cutoff, p_cutoff, p_col, ...) {
  dea_res <- ctx_get_data(ctx, "dea_res")
  contrasts <- .get_unique_contrasts(dea_res)
  for (cont in contrasts) {
    plot_name <- paste0("volcano_", cont)
    p <- glyvis::plot_volcano(
      dea_res,
      log2fc_cutoff = log2fc_cutoff,
      p_cutoff = p_cutoff,
      p_col = p_col,
      contrast = cont,
      ...
    )
    ctx <- ctx_add_plot(ctx, plot_name, p, paste0("Volcano plot for the comparison of ", cont, "."))
  }
  ctx
}

.run_step_volcano_ttest_wilcox <- function(ctx, log2fc_cutoff, p_cutoff, p_col, ...) {
  dea_res <- ctx_get_data(ctx, "dea_res")
  p <- glyvis::plot_volcano(
    dea_res,
    log2fc_cutoff = log2fc_cutoff,
    p_cutoff = p_cutoff,
    p_col = p_col,
    ...
  )
  ctx_add_plot(ctx, "volcano", p, "Volcano plot")
}

#' Get unique contrasts from DEA results
#' @param dea_res DEA results from `glystats::gly_limma()`
#' @noRd
.get_unique_contrasts <- function(dea_res) {
  checkmate::assert_class(dea_res, "glystats_limma_res")
  tidy_res <- glystats::get_tidy_result(dea_res)
  unique(paste0(tidy_res$ref_group, "_vs_", tidy_res$test_group))
}
