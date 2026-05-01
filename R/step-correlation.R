#' Step: Correlation Analysis
#'
#' @description
#' Perform pairwise correlation analysis using `glystats::gly_cor()` and
#' visualize the correlation matrix using `glyvis::plot_corrplot()`.
#' This step calculates correlation coefficients and p-values for all pairs
#' of variables or samples.
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
#' - `exp` (if `on = "exp"`): The experiment to run correlation analysis on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run correlation analysis on
#' - `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif experiment to run correlation analysis on
#' - `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif experiment to run correlation analysis on
#'
#' Tables generated (with suffixes):
#' - `correlation`: A table containing pairwise correlation results with columns:
#'   - `variable1`, `variable2` (or `sample1`, `sample2` if `on = "sample"`)
#'   - `cor`: Correlation coefficient
#'   - `p_val`: P-value from correlation test
#'   - `p_adj`: Adjusted p-value (if p_adj_method is not NULL)
#'
#' Plots generated (with suffixes):
#' - `correlation`: A correlation matrix heatmap
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step to explore relationships between variables or samples.
#' - Be careful to use when sample size or variable number is large (> 50).
#'   Before using this step for large data, ask the user if they want to proceed.
#'
#' @param on Name of the experiment to run correlation analysis on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp",
#'   "dynamic_motif_exp", "sig_dynamic_motif_exp", "branch_motif_exp", "sig_branch_motif_exp".
#' @param on_cor A character string specifying what to correlate.
#'   Either "variable" (default) to correlate variables/features, or "sample" to correlate samples.
#' @param method A character string indicating which correlation coefficient is to be computed.
#'   One of "pearson" (default) or "spearman".
#' @param p_adj_method A character string specifying the method to adjust p-values.
#'   See `p.adjust.methods` for available methods. Default is "BH".
#'   If NULL, no adjustment is performed.
#' @param plot_width Width of the plot in inches. Default is 7.
#' @param plot_height Height of the plot in inches. Default is 7.
#' @param ... Additional arguments passed to `glystats::gly_cor()`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_correlation()
#' step_correlation(on = "sig_exp")
#' step_correlation(on_cor = "sample", method = "spearman")
#' @seealso [glystats::gly_cor()], [glyvis::plot_corrplot()]
#' @export
step_correlation <- function(
  on = "exp",
  on_cor = c("variable", "sample"),
  method = c("pearson", "spearman"),
  p_adj_method = "BH",
  plot_width = 7,
  plot_height = 7,
  ...
) {
  on_cor <- rlang::arg_match(on_cor)
  method <- rlang::arg_match(method)
  signature <- rlang::expr_deparse(match.call())
  cor_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("correlation", on_meta$id_suffix)

  step(
    id = id,
    label = paste0("Correlation analysis", on_meta$label_suffix),
    run = function(ctx) {
      .run_correlation(
        ctx,
        id = id,
        on = on,
        on_cor = on_cor,
        method = method,
        p_adj_method = p_adj_method,
        plot_width = plot_width,
        plot_height = plot_height,
        cor_args = cor_args
      )
    },
    require = on,
    signature = signature,
    report = function(x) {
      .report_correlation(
        x,
        id = id,
        on = on,
        on_cor = on_cor,
        method = method
      )
    }
  )
}

#' Run correlation analysis
#'
#' @param ctx Analysis context.
#' @param id Step identifier.
#' @param on Name of the experiment data in `ctx$data`.
#' @param on_cor Whether to correlate variables or samples.
#' @param method Correlation method.
#' @param p_adj_method P-value adjustment method.
#' @param plot_width Plot width in inches.
#' @param plot_height Plot height in inches.
#' @param cor_args Additional arguments passed to `glystats::gly_cor()`.
#'
#' @returns Updated analysis context.
#' @noRd
.run_correlation <- function(
  ctx,
  id,
  on,
  on_cor,
  method,
  p_adj_method,
  plot_width,
  plot_height,
  cor_args
) {
  exp <- ctx_get_data(ctx, on)
  cor_res <- rlang::exec(
    glystats::gly_cor,
    exp,
    on = on_cor,
    method = method,
    p_adj_method = p_adj_method,
    !!!cor_args
  )

  tidy_result <- glystats::get_tidy_result(cor_res)
  ctx <- ctx_add_table(
    ctx,
    id,
    tidy_result,
    paste0("Pairwise correlation results for ", on, " (", on_cor, "s).")
  )

  p <- glyvis::plot_corrplot(cor_res)
  ctx_add_plot(
    ctx,
    id,
    p,
    paste0("Correlation matrix heatmap of ", on, " (", on_cor, "s)."),
    width = plot_width,
    height = plot_height
  )
}

#' Report correlation analysis results
#'
#' @param x Analysis context after running correlation analysis.
#' @param id Step identifier.
#' @param on Name of the experiment data in `ctx$data`.
#' @param on_cor Whether variables or samples were correlated.
#' @param method Correlation method.
#'
#' @returns A summary string for the report.
#' @noRd
.report_correlation <- function(x, id, on, on_cor, method) {
  cor_tbl <- x$tables[[id]]
  if (is.null(cor_tbl) || nrow(cor_tbl) == 0) {
    return("No correlation results available.")
  }

  median_cor <- stats::median(cor_tbl$cor, na.rm = TRUE)

  top_row <- cor_tbl |>
    dplyr::arrange(dplyr::desc(abs(.data$cor))) |>
    dplyr::slice_head(n = 1)
  highest_cor <- top_row$cor

  col1 <- if ("variable1" %in% colnames(cor_tbl)) "variable1" else "sample1"
  col2 <- if ("variable2" %in% colnames(cor_tbl)) "variable2" else "sample2"

  lines <- c(
    paste0(
      "Correlation analysis was performed on ",
      on,
      " (",
      on_cor,
      "s) using the ",
      method,
      " method."
    ),
    paste0("Number of pairs analyzed: ", nrow(cor_tbl), "."),
    paste0("Median correlation coefficient: ", round(median_cor, 3), "."),
    paste0(
      "Highest correlation: ",
      round(highest_cor, 3),
      " between ",
      top_row[[col1]],
      " and ",
      top_row[[col2]],
      "."
    )
  )
  paste(lines, collapse = "\n")
}
