#' Create analysis context
#'
#' @param exp A `glyexp_experiment`.
#' @param group_col Group column name.
#' @param dots `rlang::list2(...)`.
#'
#' @returns A context list.
#' @noRd
new_ctx <- function(exp, group_col, dots) {
  list(
    group_col = group_col,
    dots = dots,
    plots = list(),
    tables = list(),
    meta = list(explanation = list(), steps = character(0)),
    data = list(raw_exp = exp)
  )
}



#' Add a plot to context
#'
#' @param ctx Context list.
#' @param id Plot id.
#' @param p A ggplot object.
#' @param explanation Optional explanation string.
#'
#' @returns Updated context.
#' @noRd
ctx_add_plot <- function(ctx, id, p, explanation = NULL) {
  ctx$plots[[id]] <- p
  if (!is.null(explanation)) {
    ctx$meta$explanation[[paste0("plots$", id)]] <- explanation
  }
  ctx
}

#' Add a table to context
#'
#' @param ctx Context list.
#' @param id Table id.
#' @param t A tibble/data.frame.
#' @param explanation Optional explanation string.
#'
#' @returns Updated context.
#' @noRd
ctx_add_table <- function(ctx, id, t, explanation = NULL) {
  ctx$tables[[id]] <- t
  if (!is.null(explanation)) {
    ctx$meta$explanation[[paste0("tables$", id)]] <- explanation
  }
  ctx
}

#' Add a data to context
#'
#' @param ctx Context list.
#' @param id Data id.
#' @param x Any object.
#' @param explanation Optional explanation string.
#'
#' @returns Updated context.
#' @noRd
ctx_add_data <- function(ctx, id, x, explanation = NULL) {
  ctx$data[[id]] <- x
  if (!is.null(explanation)) {
    ctx$meta$explanation[[paste0("data$", id)]] <- explanation
  }
  ctx
}

#' Get data from context
#'
#' @param ctx Context list.
#' @param id Data id.
#'
#' @returns Data object.
#' @noRd
ctx_get_data <- function(ctx, id) {
  if (!id %in% names(ctx$data)) {
    cli::cli_abort("Data {.val {id}} not found in context.")
  }
  ctx$data[[id]]
}