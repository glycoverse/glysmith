#' Step: Correlation Analysis
#'
#' Perform pairwise correlation analysis using `glystats::gly_cor()` and
#' visualize the correlation matrix using `glyvis::plot_corrplot()`.
#' This step calculates correlation coefficients and p-values for all pairs
#' of variables or samples.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to run correlation analysis on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run correlation analysis on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to run correlation analysis on
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
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
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
  rlang::check_installed("Hmisc")
  rlang::check_installed("GGally")
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
      exp <- ctx_get_data(ctx, on)
      cor_res <- rlang::exec(
        glystats::gly_cor,
        exp,
        on = on_cor,
        method = method,
        p_adj_method = p_adj_method,
        !!!cor_args
      )

      # Add correlation table
      tidy_result <- glystats::get_tidy_result(cor_res)
      ctx <- ctx_add_table(
        ctx,
        id,
        tidy_result,
        paste0("Pairwise correlation results for ", on, " (", on_cor, "s).")
      )

      # Add correlation plot
      p <- glyvis::plot_corrplot(cor_res)
      ctx <- ctx_add_plot(
        ctx,
        id,
        p,
        paste0("Correlation matrix heatmap of ", on, " (", on_cor, "s)."),
        width = plot_width,
        height = plot_height
      )

      ctx
    },
    require = on,
    signature = signature,
    report = function(x) {
      cor_tbl <- x$tables[[id]]
      if (is.null(cor_tbl) || nrow(cor_tbl) == 0) {
        return("No correlation results available.")
      }

      # Calculate median correlation
      median_cor <- stats::median(cor_tbl$cor, na.rm = TRUE)

      # Find highest correlation pair
      top_row <- cor_tbl |>
        dplyr::arrange(dplyr::desc(abs(.data$cor))) |>
        dplyr::slice_head(n = 1)
      highest_cor <- top_row$cor
      item1 <- top_row[[1]]
      item2 <- top_row[[2]]

      # Determine column names based on on_cor
      col1 <- if ("variable1" %in% colnames(cor_tbl)) "variable1" else "sample1"
      col2 <- if ("variable2" %in% colnames(cor_tbl)) "variable2" else "sample2"

      lines <- c(
        paste0("Correlation analysis was performed on ", on, " (", on_cor, "s) using the ", method, " method."),
        paste0("Number of pairs analyzed: ", nrow(cor_tbl), "."),
        paste0("Median correlation coefficient: ", round(median_cor, 3), "."),
        paste0("Highest correlation: ", round(highest_cor, 3), " between ", top_row[[col1]], " and ", top_row[[col2]], ".")
      )
      paste(lines, collapse = "\n")
    }
  )
}
