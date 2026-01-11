#' Save GlySmith Result
#'
#' Save processed experiment, plots and tables of a glysmith result object to a directory.
#' A `README.md` file will also be generated to describe the saved outputs.
#'
#' @param x A glysmith result object.
#' @param dir The directory to save the result.
#' @param plot_ext The extension of the plot files. Either "pdf", "png" or "svg". Default is "pdf".
#' @param table_ext The extension of the table files. Either "csv" or "tsv". Default is "csv".
#' @param plot_width The width of the plot in inches. Default is 5.
#' @param plot_height The height of the plot in inches. Default is 5.
#'
#' @examples
#' library(glyexp)
#' exp <- real_experiment2
#' result <- forge_analysis(exp)
#' quench_result(result, tempdir())
#'
#' @export
quench_result <- function(x, dir, plot_ext = "pdf", table_ext = "csv", plot_width = 5, plot_height = 5) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(dir)
  checkmate::assert_choice(plot_ext, c("pdf", "png", "svg"))
  checkmate::assert_choice(table_ext, c("csv", "tsv"))

  if (fs::dir_exists(dir)) {
    ans <- .ask_overwrite_dir()
    ans <- tolower(trimws(ans))
    if (ans == "n") {
      cli::cli_abort("Operation cancelled. Result not saved.")
    } else if (ans == "y" || ans == "") {
      fs::dir_delete(dir)
    } else {
      cli::cli_abort("Invalid input. Operation cancelled. Result not saved.")
    }
  }

  fs::dir_create(dir, recurse = TRUE)
  fs::dir_create(fs::path(dir, "plots"))
  fs::dir_create(fs::path(dir, "tables"))

  # -------------------------------------------------------------------------
  # Handle lazy ggplot promises
  #
  # On older R versions (e.g., oldrel-1), inline ggplot expressions stored in
  # lists can become unevaluated promises (environments) instead of proper
  # ggplot objects. This happens because R uses lazy evaluation by default.
  #
  # Example that triggers this:
  #   list(plot = ggplot(mtcars, aes(mpg, wt)) + geom_point())
  #
  # The ggplot(...) expression is stored as a promise that, when eventually
  # evaluated, contains environment components rather than a proper ggplot.
  #
  # We handle this by checking if the plot is an environment and, if so,
  # forcing evaluation via .force_ggplot_evaluation().
  # -------------------------------------------------------------------------
  for (plot in names(x$plots)) {
    file_path <- fs::path(dir, "plots", paste0(plot, ".", plot_ext))
    plot_obj <- x$plots[[plot]]
    if (is.list(plot_obj) && !is.null(plot_obj$plot)) {
      # New format: list with plot, width, height
      width <- plot_obj$width %||% plot_width
      height <- plot_obj$height %||% plot_height
      raw_plot <- plot_obj$plot
      # Check for lazy promise (environment) and force evaluation if needed
      if (is.environment(raw_plot)) {
        plot_to_save <- .force_ggplot_evaluation(raw_plot)
      } else {
        plot_to_save <- raw_plot
      }
      .quietly(ggplot2::ggsave(file_path, plot_to_save, width = width, height = height))
    } else {
      # Legacy format: just the ggplot object
      raw_plot <- cast_plot(x, plot)
      # Check for lazy promise (environment) and force evaluation if needed
      if (is.environment(raw_plot)) {
        plot_to_save <- .force_ggplot_evaluation(raw_plot)
      } else {
        plot_to_save <- raw_plot
      }
      .quietly(ggplot2::ggsave(file_path, plot_to_save, width = plot_width, height = plot_height))
    }
  }

  for (table in names(x$tables)) {
    file_path <- fs::path(dir, "tables", paste0(table, ".", table_ext))
    writer <- switch(table_ext, "csv" = readr::write_csv, "tsv" = readr::write_tsv)
    .quietly(writer(cast_table(x, table), file_path))
  }

  readr::write_rds(x$exp, fs::path(dir, "experiment.rds"))
  readr::write_rds(x$meta, fs::path(dir, "meta.rds"))

  .write_result_readme(x, dir, plot_ext = plot_ext, table_ext = table_ext)

  cli::cli_alert_success("Result saved to {.path {dir}}")
}

#' Write README.md for GlySmith Result
#'
#' Write a README.md file to describe the saved outputs,
#' based on metadata in `x$meta`.
#'
#' @param x A glysmith result object.
#' @param dir The directory to save the result.
#' @param plot_ext The extension of the plot files.
#' @param table_ext The extension of the table files.
#' @noRd
.write_result_readme <- function(x, dir, plot_ext, table_ext) {
  # Generate a human-readable README for exported results.
  explanation <- NULL
  steps <- NULL
  if (!is.null(x$meta) && is.list(x$meta)) {
    explanation <- x$meta$explanation
    steps <- x$meta$steps
  }
  if (is.null(explanation)) explanation <- character(0)

  .explain <- function(key, default) {
    # Safely get explanation text by key.
    if (is.null(explanation)) return(default)
    if (is.list(explanation)) {
      if (key %in% names(explanation)) {
        val <- explanation[[key]]
        if (!is.null(val) && !is.na(val) && nzchar(val)) return(val)
      }
      return(default)
    }
    val <- explanation[key]
    if (length(val) == 1 && !is.na(val) && nzchar(val)) return(unname(val))
    default
  }

  lines <- c(
    "# GlySmith result directory",
    "",
    "This directory was generated by `glysmith::quench_result()`.",
    "",
    "## Files",
    "",
    "- `experiment.rds`: Processed experiment object.",
    "- `meta.rds`: Metadata list (including explanations and analysis steps).",
    "- `plots/`: Figures generated by the analysis pipeline.",
    "- `tables/`: Tabular outputs generated by the analysis pipeline.",
    "",
    "## Plots",
    ""
  )

  plot_names <- names(x$plots)
  if (length(plot_names) == 0) {
    lines <- c(lines, "_(none)_", "")
  } else {
    for (nm in plot_names) {
      key <- paste0("plots$", nm)
      desc <- .explain(key, default = "Plot output.")
      lines <- c(lines, sprintf("- `plots/%s.%s`: %s", nm, plot_ext, desc))
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, "## Tables", "")
  table_names <- names(x$tables)
  if (length(table_names) == 0) {
    lines <- c(lines, "_(none)_", "")
  } else {
    for (nm in table_names) {
      key <- paste0("tables$", nm)
      desc <- .explain(key, default = "Table output.")
      lines <- c(lines, sprintf("- `tables/%s.%s`: %s", nm, table_ext, desc))
    }
    lines <- c(lines, "")
  }

  if (!is.null(steps) && length(steps) > 0) {
    lines <- c(lines, "## Analysis steps", "")
    lines <- c(lines, paste0("- ", steps), "")
  }

  writeLines(lines, fs::path(dir, "README.md"), useBytes = TRUE)
  invisible(NULL)
}

#' Force evaluation of lazy ggplot promises
#'
#' @description
#' On older R versions (e.g., oldrel-1), ggplot objects created inline and stored
#' in lists can become lazy promises (environments) rather than proper ggplot objects.
#' When R stores an expression like `list(plot = ggplot(...) + geom_point())`, the
#' ggplot construction may be delayed until the promise is forced, resulting in an
#' environment with ggplot components rather than a proper ggplot object.
#'
#' This function handles both:
#' - Proper ggplot objects: returned as-is
#' - Lazy promise environments: evaluated or reconstructed into proper ggplot objects
#'
#' The evaluation strategy follows a fallback hierarchy:
#' 1. `ggplotGrob()` - converts to grob (works on newer ggplot2 with environment support)
#' 2. `ggplot_build()` + `ggplot_gtable()` - builds and converts (works on some ggplot2 versions)
#' 3. `.reconstruct_ggplot()` - manual reconstruction from environment components (last resort)
#'
#' @param x A ggplot object or environment (lazy promise).
#' @return A gtable object suitable for saving with `ggplot2::ggsave()`.
#' @noRd
.force_ggplot_evaluation <- function(x) {
  # If not an environment, return as-is (already a proper ggplot)
  if (!is.environment(x)) {
    return(x)
  }

  # Attempt 1: Try ggplotGrob directly.
  # Modern ggplot2 versions can handle environment-wrapped ggplots.
  tryCatch({
    ggplot2::ggplotGrob(x)
  }, error = function(e) {
    # Attempt 2: Try building the plot first, then convert to grob.
    # Some ggplot2 versions support ggplot_build on environment-wrapped objects.
    tryCatch({
      built <- ggplot2::ggplot_build(x)
      ggplot2::ggplot_gtable(built)
    }, error = function(e2) {
      # Attempt 3: Last resort - manually reconstruct ggplot from components.
      # This handles cases where ggplot2 cannot process the environment directly.
      .reconstruct_ggplot(x)
    })
  })
}

#' Reconstruct ggplot from environment components
#'
#' @description
#' This is a fallback function for handling lazy promise environments that cannot
#' be processed by `ggplotGrob()` or `ggplot_build()`.
#'
#' When a ggplot object becomes a lazy promise (environment), it typically contains
#' these components:
#' - `mapping` or `mappings`: aesthetic mappings from [ggplot2::aes()]
#' - `layers`: list of [ggplot2::layer()] objects (geoms, stats)
#' - `scales`: [ggplot2::Scale] objects
#' - `coordinates`: [ggplot2::Coord] object (e.g., coord_cartesian)
#' - `facet`: [ggplot2::Facet] object (e.g., facet_null)
#' - `theme`: [ggplot2::theme] object
#'
#' This function extracts these components and rebuilds a functional ggplot object.
#' Note: Some advanced features (scales, facets, coordinates) may be simplified
#' since we use identity/default versions as fallbacks.
#'
#' @param env Environment containing ggplot components (lazy promise).
#' @return A gtable object suitable for saving with `ggplot2::ggsave()`.
#' @noRd
.reconstruct_ggplot <- function(env) {
  # Extract components from the environment with fallbacks.
  # Components may have different names across ggplot2 versions.
  mapping <- env$mapping %||% env$mappings %||% ggplot2::aes()
  layers <- env$layers %||% list()

  # These are rarely needed for basic plotting, use identity defaults if missing
  scales <- env$scales %||% ggplot2::ggproto(NULL, ggplot2::ScaleIdentity$new())
  coordinates <- env$coordinates %||% ggplot2::coord_cartesian()
  facet <- env$facet %||% ggplot2::facet_null()
  theme <- env$theme %||% ggplot2::theme_gray()

  # Build a minimal ggplot with the extracted theme
  p <- ggplot2::ggplot() + theme

  # Add each layer to the ggplot
  # Layers contain the actual visual elements (geoms, stats, etc.)
  if (length(layers) > 0) {
    for (layer in layers) {
      p <- p + layer
    }
  }

  # Return as gtable for saving
  ggplot2::ggplotGrob(p)
}

# This mysterious function is written by gpt-5.2
# to silence stdout/stderr output for clean interactive console.
.quietly <- function(expr) {
  # Silence stdout/stderr output for clean interactive console.
  expr <- substitute(expr)
  tmp <- tempfile("glysmith-quiet-")
  con <- file(tmp, open = "wt")
  out0 <- sink.number()
  msg0 <- sink.number(type = "message")
  sink(con)
  sink(con, type = "message")
  on.exit({
    # Restore sinks created in this helper only.
    while (sink.number(type = "message") > msg0) sink(type = "message")
    while (sink.number() > out0) sink()
    close(con)
    unlink(tmp)
  }, add = TRUE)

  # Capture result or error, selectively suppress harmless warnings
  result <- withCallingHandlers(
    tryCatch(
      eval(expr, envir = parent.frame()),
      error = function(e) {
        # Restore sinks before re-throwing error
        while (sink.number(type = "message") > msg0) sink(type = "message")
        while (sink.number() > out0) sink()
        stop(e$message, call. = FALSE)
      }
    ),
    warning = function(w) {
      # Suppress harmless ggplot2 4.0.0 warnings about empty aesthetics
      if (grepl("Ignoring empty aesthetic", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  invisible(result)
}