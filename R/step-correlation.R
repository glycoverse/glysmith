#' Step: Correlation Analysis
#'
#' Perform pairwise correlation analysis using `glystats::gly_cor()` and
#' visualize the correlation matrix using `glyvis::plot_corrplot()`.
#' This step calculates correlation coefficients and p-values for all pairs
#' of variables or samples.
#' The upstream step supplies the experiment.
#'
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
#' step_correlation(on_cor = "sample", method = "spearman")
#' @seealso [glystats::gly_cor()], [glyvis::plot_corrplot()]
#' @export
step_correlation <- function(
  on_cor = c("variable", "sample"),
  method = c("pearson", "spearman"),
  p_adj_method = "BH",
  plot_width = 7,
  plot_height = 7,
  ...
) {
  .check_step_dots(sys.call())
  on_cor <- rlang::arg_match(on_cor)
  method <- rlang::arg_match(method)
  signature <- rlang::expr_deparse(match.call())
  cor_args <- rlang::list2(...)
  id <- "correlation"

  step(
    id = id,
    connected = .exp_port(),
    label = "Correlation analysis",
    run = function(ctx) {
      .run_correlation(
        ctx,
        id = id,
        on_cor = on_cor,
        method = method,
        p_adj_method = p_adj_method,
        plot_width = plot_width,
        plot_height = plot_height,
        cor_args = cor_args
      )
    },
    packages = c("glystats", "glyvis"),
    signature = signature,
    report = function(x) {
      .report_correlation(
        x,
        id = id,
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
  on_cor,
  method,
  p_adj_method,
  plot_width,
  plot_height,
  cor_args
) {
  exp <- ctx$data$exp
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
    paste0(
      "Pairwise correlation results for the input experiment (",
      on_cor,
      "s)."
    )
  )

  p <- glyvis::plot_corrplot(cor_res)
  ctx_add_plot(
    ctx,
    id,
    p,
    paste0(
      "Correlation matrix heatmap of the input experiment (",
      on_cor,
      "s)."
    ),
    width = plot_width,
    height = plot_height
  )
}

#' Report correlation analysis results
#'
#' @param x Analysis context after running correlation analysis.
#' @param id Step identifier.
#' @param on_cor Whether variables or samples were correlated.
#' @param method Correlation method.
#'
#' @returns A summary string for the report.
#' @noRd
.report_correlation <- function(x, id, on_cor, method) {
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
      "the input experiment",
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
