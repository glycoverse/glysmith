#' Build or retrieve a glyclean preprocessing pipeline
#'
#' `step_build_pipeline()` builds a [glyclean::glyclean_pipeline()] from
#' the supplied calls. `step_get_pipeline()` retrieves the active experiment's
#' processing history with [glyclean::get_pipeline()]. Both store the pipeline
#' under `pipeline` in the result data, where [cast_data()] can retrieve it
#' for another experiment.
#'
#' @param ... Calls to glyclean functions with the experiment argument omitted
#'   and any other arguments named. Parameter values are captured when the
#'   blueprint is built.
#' @returns A `glysmith_step` object.
#' @examplesIf requireNamespace("glyclean", quietly = TRUE)
#' blueprint(
#'   step_build_pipeline(glyclean::zero_to_na(), glyclean::impute_min_prob(seed = 42)),
#'   step_apply_pipeline()
#' )
#' blueprint(step_normalize("median"), step_get_pipeline())
#' @export
step_build_pipeline <- function(...) {
  # An empty template can be constructed without the optional dependency.
  pipeline <- if (...length()) glyclean::glyclean_pipeline(...)
  step(
    id = "build_pipeline",
    label = "Build a preprocessing pipeline",
    run = function(ctx) {
      ctx_add_data(ctx, "pipeline", pipeline %||% glyclean::glyclean_pipeline())
    },
    generate = "pipeline",
    packages = "glyclean",
    stop_on_error = TRUE,
    signature = rlang::expr_deparse(match.call())
  )
}

#' @rdname step_build_pipeline
#' @export
step_get_pipeline <- function() {
  step(
    id = "get_pipeline",
    label = "Save preprocessing history",
    run = function(ctx) {
      ctx_add_data(
        ctx,
        "pipeline",
        glyclean::get_pipeline(ctx_get_data(ctx, "exp"))
      )
    },
    require = "exp",
    generate = "pipeline",
    packages = "glyclean",
    stop_on_error = TRUE,
    signature = rlang::expr_deparse(match.call())
  )
}

#' Apply a glyclean pipeline to the active experiment
#'
#' Replay a constructed or recorded pipeline with [glyclean::apply_pipeline()].
#' The processed experiment becomes the input to the next step.
#'
#' @param pipeline A `glyclean_pipeline` object. If omitted, use the pipeline
#'   stored by [step_build_pipeline()] or [step_get_pipeline()].
#' @returns A `glysmith_step` object.
#' @examplesIf requireNamespace("glyclean", quietly = TRUE)
#' pipeline <- glyclean::glyclean_pipeline(glyclean::normalize_total_area())
#' blueprint(step_apply_pipeline(pipeline), step_pca())
#' @export
step_apply_pipeline <- function(pipeline = NULL) {
  if (!is.null(pipeline)) {
    checkmate::assert_class(pipeline, "glyclean_pipeline")
  }
  .clean_step(
    "apply_pipeline",
    "Apply a preprocessing pipeline",
    args = if (is.null(pipeline)) list() else list(pipeline = pipeline),
    data_args = if (is.null(pipeline)) "pipeline" else character()
  )
}
