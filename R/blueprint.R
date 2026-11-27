#' Compose a tree of analytical steps
#'
#' A step receives its predecessor's experiment or fitted result. A terminal
#' step emits only reporting artifacts. Use [fork()] for siblings and [br()]
#' for a side route that leaves the current connection available.
#' See `vignette("blueprint")` for a complete analysis.
#' @param ... Steps, side branches, forks or reusable blueprints.
#' @returns A validated blueprint. Printing it shows each producer and consumer.
#' @examples
#' blueprint(step_preprocess(), fork(step_pca(), step_dea_limma()))
#' @export
blueprint <- function(...) {
  bp <- structure(
    rlang::list2(...),
    class = "glysmith_blueprint",
    format_version = 2L
  )
  if (!length(bp)) {
    cli::cli_abort("A blueprint needs at least one step.")
  }
  .compile_blueprint(bp)
  bp
}

#' Save or Load a Blueprint
#'
#' - `write_blueprint()` saves a blueprint to a RDS file.
#' - `read_blueprint()` loads a blueprint from a RDS file.
#'
#' @param bp A [blueprint()].
#' @param file A character string giving the name of the file to save to or load from.
#'
#' @returns Invisibly returns the blueprint object.
#' @examples
#' bp <- blueprint(
#'   step_preprocess(),
#'   fork(step_pca(), step_dea_limma()),
#' )
#' write_blueprint(bp, tempfile(fileext = ".rds"))
#' @export
write_blueprint <- function(bp, file) {
  .assert_blueprint(bp)
  saveRDS(bp, file)
}

#' @rdname write_blueprint
#' @export
read_blueprint <- function(file) {
  x <- readRDS(file)
  .assert_blueprint(x)
  x
}

.assert_blueprint <- function(x) {
  if (
    !inherits(x, "glysmith_blueprint") ||
      !identical(attr(x, "format_version"), 2L)
  ) {
    cli::cli_abort(
      "Incompatible blueprint. Rebuild it with blueprint(), br() and fork()."
    )
  }
}

.run_blueprint_run_once <- function(step, ctx) {
  tryCatch(
    {
      new_ctx <- NULL
      logs_warning <- list()
      logs_output <- utils::capture.output(
        {
          logs_message <- utils::capture.output(
            {
              new_ctx <- withCallingHandlers(
                step$run(ctx),
                warning = function(w) {
                  logs_warning[[length(logs_warning) + 1]] <<- w
                  invokeRestart("muffleWarning")
                }
              )
            },
            type = "message"
          )
        },
        type = "output"
      )

      list(
        status = "success",
        new_ctx = new_ctx,
        logs = list(
          output = logs_output,
          message = logs_message,
          warning = logs_warning
        )
      )
    },
    error = function(e) {
      list(status = "error", error = e)
    }
  )
}
