#' Step: Cox Proportional Hazards Model
#'
#' Perform survival analysis by fitting a Cox proportional hazards model
#' using `glystats::gly_cox()` for each variable.
#' This step identifies variables associated with survival outcomes.
#' The upstream step supplies the experiment.
#'
#' @param time_col Column name in sample information containing survival time.
#'   Default is "time".
#' @param event_col Column name in sample information containing event indicator
#'   (1 for event, 0 for censoring). Default is "event".
#' @param p_adj_method Method for adjusting p-values. See `p.adjust.methods`.
#'   Default is "BH". If NULL, no adjustment is performed.
#' @param ... Additional arguments passed to `glystats::gly_cox()`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_cox()
#' step_cox(time_col = "survival_time", event_col = "death")
#' @seealso [glystats::gly_cox()], [survival::coxph()]
#' @export
step_cox <- function(
  time_col = "time",
  event_col = "event",
  p_adj_method = "BH",
  ...
) {
  .check_step_dots(sys.call())
  signature <- rlang::expr_deparse(match.call())
  cox_args <- rlang::list2(...)
  id <- "cox"

  step(
    id = id,
    connected = .exp_port(),
    label = "Cox proportional hazards model",
    run = function(ctx) {
      .run_cox(
        ctx,
        id = id,
        time_col = time_col,
        event_col = event_col,
        p_adj_method = p_adj_method,
        cox_args = cox_args
      )
    },
    packages = c("glystats", "survival"),
    signature = signature,
    report = function(x) {
      .report_cox(x, id = id)
    },
    condition = function(ctx) {
      .condition_cox(ctx, time_col = time_col, event_col = event_col)
    }
  )
}

#' Run Cox proportional hazards analysis
#'
#' @param ctx Analysis context.
#' @param id Step identifier.
#' @param time_col Survival time column.
#' @param event_col Event indicator column.
#' @param p_adj_method P-value adjustment method.
#' @param cox_args Additional arguments passed to `glystats::gly_cox()`.
#'
#' @returns Updated analysis context.
#' @noRd
.run_cox <- function(
  ctx,
  id,
  time_col,
  event_col,
  p_adj_method,
  cox_args
) {
  exp <- ctx$data$exp
  cox_res <- rlang::exec(
    glystats::gly_cox,
    exp,
    time_col = time_col,
    event_col = event_col,
    p_adj_method = p_adj_method,
    !!!cox_args
  )
  tidy_result <- glystats::get_tidy_result(cox_res)
  ctx <- ctx_add_data(ctx, paste0(id, "_id_column"), .stats_id_column(cox_res))
  ctx <- ctx_add_table(
    ctx,
    id,
    tidy_result,
    paste0("Cox regression results for the input experiment.")
  )
  ctx_add_data(ctx, paste0(id, "_raw_res"), cox_res$raw_result)
}

#' Report Cox proportional hazards analysis results
#'
#' @param x Analysis context after running Cox analysis.
#' @param id Step identifier.
#'
#' @returns A summary string for the report.
#' @noRd
.report_cox <- function(x, id) {
  cox_tbl <- x$tables[[id]]
  if (is.null(cox_tbl) || nrow(cox_tbl) == 0) {
    return("No Cox regression results available.")
  }

  n_sig <- sum(cox_tbl$p_adj < 0.05, na.rm = TRUE)
  n_total <- nrow(cox_tbl)

  top_row <- cox_tbl |>
    dplyr::arrange(.data$p_adj) |>
    dplyr::slice_head(n = 1)
  id_col <- x$data[[paste0(id, "_id_column")]] %||% "variable"
  top_var <- top_row[[id_col]]
  top_hr <- top_row$hr
  top_p_adj <- top_row$p_adj

  lines <- c(
    paste0(
      "Cox proportional hazards model was performed on the input experiment."
    ),
    paste0("Number of variables analyzed: ", n_total, "."),
    paste0(
      "Number of significant variables (adjusted p < 0.05): ",
      n_sig,
      "."
    ),
    paste0(
      "Top associated variable: ",
      top_var,
      " (HR = ",
      round(top_hr, 3),
      ", adjusted p = ",
      format(top_p_adj, scientific = TRUE, digits = 3),
      ")."
    )
  )
  paste(lines, collapse = "\n")
}

#' Check whether Cox analysis should run
#'
#' @param ctx Analysis context.
#' @param time_col Survival time column.
#' @param event_col Event indicator column.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_cox <- function(ctx, time_col, event_col) {
  exp <- tryCatch(ctx$data$exp, error = function(e) NULL)
  if (is.null(exp)) {
    return(list(check = FALSE, reason = "Experiment not found."))
  }
  sample_info <- .get_sample_info(exp)
  if (!time_col %in% colnames(sample_info)) {
    return(list(
      check = FALSE,
      reason = paste0(
        "Time column '",
        time_col,
        "' not found in sample info."
      )
    ))
  }
  if (!event_col %in% colnames(sample_info)) {
    return(list(
      check = FALSE,
      reason = paste0(
        "Event column '",
        event_col,
        "' not found in sample info."
      )
    ))
  }
  list(check = TRUE, reason = NULL)
}
