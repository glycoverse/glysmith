#' Step: Cox Proportional Hazards Model
#'
#' Perform survival analysis by fitting a Cox proportional hazards model
#' using `glystats::gly_cox()` for each variable.
#' This step identifies variables associated with survival outcomes.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to run Cox regression on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run Cox regression on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to run Cox regression on
#'
#' The experiment must contain survival data with `time_col` and `event_col` columns
#' in the sample information.
#'
#' Tables generated (with suffixes):
#' - `cox`: A table containing Cox regression results with columns:
#'   - `variable`: Variable name
#'   - `coefficient`: Regression coefficient (log hazard ratio)
#'   - `std.error`: Standard error of the coefficient
#'   - `statistic`: Wald test statistic
#'   - `p_val`: Raw p-value from Wald test
#'   - `hr`: Hazard ratio (exp(coefficient))
#'   - `p_adj`: Adjusted p-value (if p_adj_method is not NULL)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step when users want to identify variables associated with survival outcomes.
#' - This step requires survival data (time and event columns) in the sample information.
#' - Always ask for the column names for survival data, unless explicitly provided.
#'
#' @param on Name of the experiment to run Cox regression on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
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
#' step_cox(on = "sig_exp", p_adj_method = "bonferroni")
#' @seealso [glystats::gly_cox()], [survival::coxph()]
#' @export
step_cox <- function(
  on = "exp",
  time_col = "time",
  event_col = "event",
  p_adj_method = "BH",
  ...
) {
  signature <- rlang::expr_deparse(match.call())
  cox_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("cox", on_meta$id_suffix)

  step(
    id = id,
    label = paste0("Cox proportional hazards model", on_meta$label_suffix),
    run = function(ctx) {
      rlang::check_installed("survival")
      exp <- ctx_get_data(ctx, on)
      cox_res <- rlang::exec(
        glystats::gly_cox,
        exp,
        time_col = time_col,
        event_col = event_col,
        p_adj_method = p_adj_method,
        !!!cox_args
      )
      tidy_result <- glystats::get_tidy_result(cox_res)
      ctx <- ctx_add_table(
        ctx,
        id,
        tidy_result,
        paste0("Cox regression results for ", on, ".")
      )
      ctx <- ctx_add_data(ctx, paste0(id, "_raw_res"), cox_res$raw_result)
      ctx
    },
    require = on,
    signature = signature,
    report = function(x) {
      cox_tbl <- x$tables[[id]]
      if (is.null(cox_tbl) || nrow(cox_tbl) == 0) {
        return("No Cox regression results available.")
      }

      n_sig <- sum(cox_tbl$p_adj < 0.05, na.rm = TRUE)
      n_total <- nrow(cox_tbl)

      # Find most significant variable
      top_row <- cox_tbl |>
        dplyr::arrange(.data$p_adj) |>
        dplyr::slice_head(n = 1)
      top_var <- top_row$variable
      top_hr <- top_row$hr
      top_p_adj <- top_row$p_adj

      lines <- c(
        paste0("Cox proportional hazards model was performed on ", on, "."),
        paste0("Number of variables analyzed: ", n_total, "."),
        paste0("Number of significant variables (adjusted p < 0.05): ", n_sig, "."),
        paste0("Top associated variable: ", top_var, " (HR = ", round(top_hr, 3), ", adjusted p = ", format(top_p_adj, scientific = TRUE, digits = 3), ").")
      )
      paste(lines, collapse = "\n")
    },
    condition = function(ctx) {
      exp <- tryCatch(ctx_get_data(ctx, on), error = function(e) NULL)
      if (is.null(exp)) {
        return(list(check = FALSE, reason = "Experiment not found."))
      }
      sample_info <- glyexp::get_sample_info(exp)
      if (!time_col %in% colnames(sample_info)) {
        return(list(check = FALSE, reason = paste0("Time column '", time_col, "' not found in sample info.")))
      }
      if (!event_col %in% colnames(sample_info)) {
        return(list(check = FALSE, reason = paste0("Event column '", event_col, "' not found in sample info.")))
      }
      list(check = TRUE, reason = NULL)
    }
  )
}
