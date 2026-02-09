#' Create analysis context
#'
#' @param exp A `glyexp_experiment`.
#' @param group_col Group column name.
#'
#' @returns A context list.
#' @noRd
new_ctx <- function(exp, group_col) {
  list(
    group_col = group_col,
    plots = list(),
    tables = list(),
    meta = list(explanation = list(), steps = character(0)),
    data = list(exp = exp)
  )
}


#' Add a plot to context
#'
#' @param ctx Context list.
#' @param id Plot id.
#' @param p A ggplot object.
#' @param explanation Optional explanation string.
#' @param width Plot width in inches for export. Default is NULL (uses global default).
#' @param height Plot height in inches for export. Default is NULL (uses global default).
#'
#' @returns Updated context.
#' @noRd
ctx_add_plot <- function(
  ctx,
  id,
  p,
  explanation = NULL,
  width = NULL,
  height = NULL
) {
  plot_info <- list(plot = p)
  if (!is.null(width)) {
    plot_info$width <- width
  }
  if (!is.null(height)) {
    plot_info$height <- height
  }
  ctx$plots[[id]] <- plot_info
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

#' Delete data from context
#'
#' @param ctx Context list.
#' @param id Data id.
#'
#' @returns Updated context.
#' @noRd
ctx_delete_data <- function(ctx, id) {
  ctx$data[[id]] <- NULL
  ctx
}
