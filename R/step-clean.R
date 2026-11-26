#' Normalize sample abundances
#'
#' Use [glyclean::auto_normalize()] or select a `normalize_*` function by
#' its suffix. The step updates the active experiment (`exp`). The first
#' cleaning step saves its input in `raw_exp`.
#'
#' @param method Use `"auto"` to choose by experiment type, or select
#'   `"median"`, `"median_abs"`, `"median_quotient"`, `"total_area"`,
#'   `"quantile"`, `"loessf"`, `"loesscyc"`, `"rlr"`, `"rlrma"`,
#'   `"rlrmacyc"` or `"vsn"`.
#' @param ... Arguments for the selected glyclean function. For example,
#'   [glyclean::normalize_median_quotient()] accepts `by` to normalize
#'   within groups, and [glyclean::normalize_rlrmacyc()] accepts `n_iter`.
#' @returns A `glysmith_step` object.
#' @examples
#' blueprint(step_normalize("median_quotient", by = "group"), step_pca())
#' @export
step_normalize <- function(
  method = c(
    "auto",
    "median",
    "median_abs",
    "median_quotient",
    "total_area",
    "quantile",
    "loessf",
    "loesscyc",
    "rlr",
    "rlrma",
    "rlrmacyc",
    "vsn"
  ),
  ...
) {
  method <- rlang::arg_match(method)
  .clean_step(
    .glyclean_method("normalize", method),
    paste0("Normalize abundances (", method, ")"),
    rlang::list2(...),
    packages = switch(method, vsn = "vsn", rlr = , rlrma = , rlrmacyc = "MASS")
  )
}

#' Impute missing abundances
#'
#' Use [glyclean::auto_impute()] or select an `impute_*` function by its
#' suffix. The imputed experiment becomes the input to the next step.
#'
#' @param method Imputation method. `"auto"` selects by experiment type and
#'   sample count. Explicit methods are `"zero"`, `"sample_min"`,
#'   `"half_sample_min"`, `"sw_knn"`, `"fw_knn"`, `"bpca"`, `"ppca"`,
#'   `"svd"`, `"min_prob"`, `"miss_forest"` and `"dreamai"`. DreamAI
#'   requires a glyclean installation that exports `impute_dreamai()`.
#' @param ... Arguments for the selected glyclean function, such as `k` for
#'   nearest-neighbor imputation, or `by`, `q`, `tune.sigma` and `seed`
#'   for [glyclean::impute_min_prob()].
#' @param dreamai_methods Components passed to the DreamAI ensemble's
#'   `method` argument. The default uses glyclean's choice of components.
#' @returns A `glysmith_step` object.
#' @examples
#' step_impute("half_sample_min")
#' step_impute("min_prob", by = "group", q = 0.02, seed = 42)
#' @export
step_impute <- function(
  method = c(
    "auto",
    "zero",
    "sample_min",
    "half_sample_min",
    "sw_knn",
    "fw_knn",
    "bpca",
    "ppca",
    "svd",
    "min_prob",
    "miss_forest",
    "dreamai"
  ),
  ...,
  dreamai_methods = NULL
) {
  method <- rlang::arg_match(method)
  args <- rlang::list2(...)
  if (!is.null(dreamai_methods)) {
    if (method != "dreamai") {
      cli::cli_abort(
        "{.arg dreamai_methods} requires {.code method = \"dreamai\"}."
      )
    }
    args$method <- dreamai_methods
  }
  .clean_step(
    .glyclean_method("impute", method),
    paste0("Impute missing abundances (", method, ")"),
    args,
    packages = switch(method, sw_knn = , fw_knn = "impute", dreamai = "callr")
  )
}

#' Filter features by abundance or missingness
#'
#' Use [glyclean::auto_remove()] or select a `remove_*` function by its
#' suffix. Chain filters in the order in which their criteria should apply.
#'
#' @param method Filtering method. Use `"auto"` for a preset, `"rare"` for
#'   missingness, `"constant"` for invariant features, or `"low_var"`,
#'   `"low_cv"` and `"low_expr"` for variance, coefficient of variation
#'   and abundance thresholds.
#' @param ... Arguments for the selected filter. For example,
#'   [glyclean::remove_rare()] takes `prop`, `n`, `min_n`, `by` and
#'   `strict`. [glyclean::auto_remove()] takes `preset` and `group_col`.
#' @returns A `glysmith_step` object.
#' @examples
#' blueprint(step_remove("rare", prop = 0.4), step_remove("constant"))
#' @export
step_remove <- function(
  method = c("auto", "rare", "constant", "low_var", "low_cv", "low_expr"),
  ...
) {
  method <- rlang::arg_match(method)
  .clean_step(
    .glyclean_method("remove", method),
    paste0("Filter features (", method, ")"),
    rlang::list2(...)
  )
}

#' Convert zero abundances to missing values
#'
#' Apply [glyclean::zero_to_na()] to the active experiment before filtering
#' or imputing measurements encoded as zero.
#'
#' @returns A `glysmith_step` object.
#' @examples
#' blueprint(step_zero_to_na(), step_remove("rare"), step_impute("min_prob"))
#' @export
step_zero_to_na <- function() {
  .clean_step("zero_to_na", "Convert zeros to missing values")
}

#' Aggregate abundances by glycan or glycopeptide identifier
#'
#' Apply [glyclean::aggregate()] to the active experiment. With
#' `to_level = NULL`, glyclean selects the level from the experiment type
#' and available structural annotations, as in its former `auto_aggregate()`.
#'
#' @inheritParams glyclean::aggregate
#' @returns A `glysmith_step` object.
#' @examples
#' step_aggregate()
#' step_aggregate(to_level = "gf", f = mean, standardize_variable = FALSE)
#' @export
step_aggregate <- function(
  to_level = NULL,
  standardize_variable = TRUE,
  f = sum,
  seed = 123
) {
  .clean_step(
    "aggregate",
    "Aggregate abundances",
    list(
      to_level = to_level,
      standardize_variable = standardize_variable,
      f = f,
      seed = seed
    )
  )
}

#' Correct batch effects
#'
#' Apply [glyclean::auto_correct_batch_effect()] to correct batches when the
#' proportion of affected features exceeds its threshold. Select ComBat or
#' limma to apply [glyclean::correct_batch_effect()] directly.
#'
#' @param method Batch correction method, one of `"auto"`, `"combat"` or `"limma"`.
#' @param ... Arguments for the selected method. Automatic correction uses
#'   `batch_col`, `group_col` and `prop_threshold`. ComBat and limma use
#'   `batch` and `group`. Both functions accept `check_confounding` and
#'   `confounding_threshold`.
#' @returns A `glysmith_step` object.
#' @examples
#' step_correct_batch_effect("auto", batch_col = "plate", prop_threshold = 0.2)
#' step_correct_batch_effect("limma", batch = "plate", group = "group")
#' @export
step_correct_batch_effect <- function(
  method = c("auto", "combat", "limma"),
  ...
) {
  method <- rlang::arg_match(method)
  args <- rlang::list2(...)
  operation <- "auto_correct_batch_effect"
  if (method != "auto") {
    operation <- "correct_batch_effect"
    args$method <- method
  }
  .clean_step(
    operation,
    paste0("Correct batch effects (", method, ")"),
    args,
    id = paste0("correct_batch_effect_", method)
  )
}

#' Test for batch effects in each feature
#'
#' Run [glyclean::detect_batch_effect()] on the active experiment and save
#' the feature identifiers (`.variable`) and ANOVA p-values (`p_value`) in
#' the `batch_effect` result table.
#'
#' @inheritParams glyclean::detect_batch_effect
#' @returns A `glysmith_step` object.
#' @examples
#' step_detect_batch_effect(batch = "plate", group = "group")
#' @export
step_detect_batch_effect <- function(batch = "batch", group = NULL) {
  step(
    id = "detect_batch_effect",
    label = "Detect batch effects",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      p <- glyclean::detect_batch_effect(exp, batch = batch, group = group)
      ids <- names(p) %||% rownames(exp) %||% rep(NA_character_, length(p))
      tbl <- tibble::tibble(.variable = ids, p_value = unname(p))
      ctx_add_table(ctx, "batch_effect", tbl, "Batch effect ANOVA by feature.")
    },
    require = "exp",
    packages = "glyclean",
    signature = rlang::expr_deparse(match.call())
  )
}

#' Log-transform abundances
#'
#' Apply [glyclean::transform_log()] to the active experiment. Glyclean
#' converts from the stored abundance scale and records the new scale.
#' For log-ratio coordinates use [step_coda()].
#'
#' @inheritParams glyclean::transform_log
#' @returns A `glysmith_step` object.
#' @examples
#' step_transform_log(base = 10)
#' @export
step_transform_log <- function(base = 2) {
  .clean_step("transform_log", "Log-transform abundances", list(base = base))
}

#' Annotate the sequence surrounding each glycosylation site
#'
#' Apply [glyclean::add_site_seq()] to a glycoproteomics experiment and
#' store the flanking sequence in the `site_sequence` feature annotation.
#'
#' @inheritParams glyclean::add_site_seq
#' @returns A `glysmith_step` object.
#' @examples
#' step_add_site_seq(fasta = c(P12345 = "MANVTK"), n_aa = 2)
#' @export
step_add_site_seq <- function(fasta = NULL, n_aa = 7, taxid = 9606) {
  .clean_step(
    "add_site_seq",
    "Annotate glycosylation site sequences",
    list(fasta = fasta, n_aa = n_aa, taxid = taxid)
  )
}

.glyclean_method <- function(family, method) {
  if (method == "auto") paste0("auto_", family) else paste0(family, "_", method)
}

# Resolve the optional package at execution time so blueprints can be
# constructed and inspected without loading glyclean.
.clean_step <- function(
  operation,
  label,
  args = list(),
  id = operation,
  data_args = character(),
  packages = character(),
  call = rlang::caller_call()
) {
  force(args)
  step(
    id = id,
    label = label,
    run = function(ctx) {
      if (!operation %in% getNamespaceExports("glyclean")) {
        cli::cli_abort(
          "This step requires a version of {.pkg glyclean} that exports {.fn {operation}}."
        )
      }
      fun <- getExportedValue("glyclean", operation)
      input <- rlang::set_names(
        list(ctx_get_data(ctx, "exp")),
        names(formals(fun))[1L]
      )
      result <- rlang::exec(fun, !!!input, !!!args, !!!ctx$data[data_args])
      .store_preprocessed_exp(ctx, result)
    },
    require = c("exp", data_args),
    initialize = "raw_exp",
    packages = c("glyclean", packages),
    repeatable = TRUE,
    stop_on_error = TRUE,
    signature = rlang::expr_deparse(call)
  )
}

.store_preprocessed_exp <- function(ctx, exp) {
  if (is.null(ctx$data$raw_exp)) {
    ctx <- ctx_add_data(ctx, "raw_exp", ctx_get_data(ctx, "exp"))
  }
  ctx_add_data(ctx, "exp", exp)
}
