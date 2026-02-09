#' Create a Branch in a Blueprint
#'
#' Use `br()` to group steps that should run as an isolated branch with
#' namespaced outputs prefixed by `<name>__`.
#'
#' @param name Branch name used as a prefix for outputs.
#' @param ... One or more step objects.
#'
#' @returns A branch object used inside [blueprint()].
#' @examples
#' blueprint(
#'   step_preprocess(),
#'   br("limma",
#'     step_dea_limma(),
#'     step_volcano()
#'   ),
#'   br("ttest",
#'     step_dea_ttest(),
#'     step_volcano()
#'   )
#' )
#' @export
br <- function(name, ...) {
  checkmate::assert_string(name, min.chars = 1)
  steps <- as.list(rlang::list2(...))
  structure(list(name = name, steps = steps), class = "glysmith_branch")
}

.expand_branches <- function(steps) {
  out <- list()
  for (item in steps) {
    if (inherits(item, "glysmith_branch")) {
      out <- append(out, .branch_to_steps(item))
    } else {
      out <- append(out, list(item))
    }
  }
  out
}

.branch_to_steps <- function(branch) {
  name <- branch$name
  steps <- branch$steps %||% list()
  if (length(steps) == 0) {
    return(list())
  }

  purrr::map(steps, function(step) {
    if (inherits(step, "glysmith_branch")) {
      cli::cli_abort("Nested branches are not supported.")
    }
    if (!inherits(step, "glysmith_step")) {
      cli::cli_abort("Invalid step object.")
    }
    .branch_wrap_step(step, name)
  })
}

.branch_wrap_step <- function(step, branch) {
  prefix <- .branch_prefix(branch)
  original_id <- step$id
  wrapped_id <- paste0(prefix, original_id)

  require <- .branch_prefix_keys(step$require %||% character(0), prefix)
  generate <- .branch_prefix_keys(step$generate %||% character(0), prefix)

  condition <- NULL
  if (is.function(step$condition)) {
    condition <- function(ctx) {
      .branch_run_condition(step, ctx, branch)
    }
  }

  report <- NULL
  if (is.function(step$report)) {
    report <- function(x) {
      .branch_run_report(step, x, branch, original_id, wrapped_id)
    }
  }

  wrapped <- step(
    id = wrapped_id,
    label = paste0("[", branch, "] ", step$label),
    run = function(ctx) {
      .branch_run_step(step, ctx, branch)
    },
    report = report,
    require = require,
    generate = generate,
    condition = condition,
    retry = step$retry %||% 0L,
    signature = paste0("br(\"", branch, "\", ", step$signature, ")")
  )

  wrapped$branch <- branch
  wrapped$branch_signature <- step$signature
  wrapped
}

.branch_run_condition <- function(step, ctx, branch) {
  prefix <- .branch_prefix(branch)
  temp_ctx <- .branch_overlay_ctx(ctx, prefix)
  required <- step$require %||% character(0)
  if (length(required) > 0) {
    missing <- setdiff(required, names(temp_ctx$data))
    if (length(missing) > 0) {
      reason <- paste0(
        "missing required data: ",
        paste(missing, collapse = ", ")
      )
      return(list(check = FALSE, reason = reason))
    }
  }
  step$condition(temp_ctx)
}

.branch_run_step <- function(step, ctx, branch) {
  prefix <- .branch_prefix(branch)

  temp_ctx <- .branch_overlay_ctx(ctx, prefix)
  pre_data <- temp_ctx$data %||% list()
  pre_plots <- temp_ctx$plots %||% list()
  pre_tables <- temp_ctx$tables %||% list()
  pre_expl <- temp_ctx$meta$explanation %||% list()

  post_ctx <- step$run(temp_ctx)
  post_data <- post_ctx$data %||% list()
  post_plots <- post_ctx$plots %||% list()
  post_tables <- post_ctx$tables %||% list()
  post_expl <- post_ctx$meta$explanation %||% list()

  ctx$data <- .branch_update_list(
    ctx$data %||% list(),
    pre_data,
    post_data,
    prefix
  )
  ctx$plots <- .branch_update_list(
    ctx$plots %||% list(),
    pre_plots,
    post_plots,
    prefix
  )
  ctx$tables <- .branch_update_list(
    ctx$tables %||% list(),
    pre_tables,
    post_tables,
    prefix
  )

  ctx$meta <- ctx$meta %||% list()
  ctx$meta$explanation <- .branch_update_explanations(
    ctx$meta$explanation %||% list(),
    pre_expl,
    post_expl,
    prefix
  )

  ctx
}

.branch_run_report <- function(step, x, branch, original_id, wrapped_id) {
  x_branch <- .branch_view_result(x, branch, original_id, wrapped_id)
  step$report(x_branch)
}

.branch_view_result <- function(x, branch, original_id, wrapped_id) {
  prefix <- .branch_prefix(branch)
  x_branch <- x

  x_branch$data <- x$data %||% list()
  x_branch$plots <- x$plots %||% list()
  x_branch$tables <- x$tables %||% list()
  x_branch$meta <- x$meta %||% list()
  x_branch$meta$logs <- x_branch$meta$logs %||% list()

  if (!is.null(x_branch$meta$logs[[wrapped_id]])) {
    x_branch$meta$logs[[original_id]] <- x_branch$meta$logs[[wrapped_id]]
  }

  if (!is.null(x_branch$data[[paste0(prefix, "exp")]])) {
    x_branch$exp <- x_branch$data[[paste0(prefix, "exp")]]
  }

  x_branch$data <- .branch_unprefix_list(x_branch$data, prefix)
  x_branch$plots <- .branch_unprefix_list(x_branch$plots, prefix)
  x_branch$tables <- .branch_unprefix_list(x_branch$tables, prefix)

  x_branch
}

.branch_overlay_ctx <- function(ctx, prefix) {
  data <- ctx$data %||% list()
  if (length(data) > 0) {
    branch_keys <- names(data)
    branch_keys <- branch_keys[startsWith(branch_keys, prefix)]
    for (k in branch_keys) {
      unprefixed <- substr(k, nchar(prefix) + 1, nchar(k))
      data[[unprefixed]] <- data[[k]]
    }
  }
  ctx$data <- data
  ctx
}

.branch_unprefix_list <- function(x, prefix) {
  keys <- names(x)
  if (length(keys) == 0) {
    return(x)
  }
  branch_keys <- keys[startsWith(keys, prefix)]
  for (k in branch_keys) {
    unprefixed <- substr(k, nchar(prefix) + 1, nchar(k))
    x[[unprefixed]] <- x[[k]]
  }
  x
}

.branch_update_list <- function(target, before, after, prefix) {
  diff <- .branch_diff_keys(before, after)

  for (k in diff$changed) {
    prefixed <- .branch_prefix_key(prefix, k)
    target[[prefixed]] <- after[[k]]
  }

  for (k in diff$removed) {
    prefixed <- .branch_prefix_key(prefix, k)
    target[[prefixed]] <- NULL
  }

  target
}

.branch_update_explanations <- function(target, before, after, prefix) {
  diff <- .branch_diff_keys(before, after)
  keys <- c(diff$changed, diff$removed)

  for (key in keys) {
    if (!stringr::str_detect(key, "^(data|plots|tables)\\$")) {
      next
    }
    type <- stringr::str_replace(key, "\\$.*$", "")
    id <- stringr::str_replace(key, "^[^$]+\\$", "")
    prefixed <- paste0(type, "$", .branch_prefix_key(prefix, id))

    if (key %in% diff$removed) {
      target[[prefixed]] <- NULL
    } else {
      target[[prefixed]] <- after[[key]]
    }
  }

  target
}

.branch_diff_keys <- function(before, after) {
  before <- before %||% list()
  after <- after %||% list()

  before_names <- names(before) %||% character(0)
  after_names <- names(after) %||% character(0)

  added <- setdiff(after_names, before_names)
  removed <- setdiff(before_names, after_names)
  common <- intersect(before_names, after_names)
  changed <- common[
    !vapply(common, function(k) identical(before[[k]], after[[k]]), logical(1))
  ]

  list(changed = unique(c(added, changed)), removed = removed)
}

.branch_prefix_keys <- function(keys, prefix) {
  shared <- .branch_shared_keys()
  if (length(keys) == 0) {
    return(character(0))
  }
  purrr::map_chr(keys, function(key) {
    if (key %in% shared) key else .branch_prefix_key(prefix, key)
  })
}

.branch_prefix_key <- function(prefix, key) {
  if (startsWith(key, prefix)) {
    key
  } else {
    paste0(prefix, key)
  }
}

.branch_prefix <- function(name) {
  paste0(name, "__")
}

.branch_shared_keys <- function() {
  "exp"
}
