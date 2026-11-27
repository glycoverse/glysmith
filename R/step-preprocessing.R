#' Step: Plot QC
#'
#' @description
#' Generate quality control plots for the experiment using `glyclean` plotting functions.
#' This step can be used before AND after `step_preprocess()` to generate QC plots at different stages.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment to plot QC for
#'
#' The default panel (`method = NULL`) generates the following plots.
#' - `qc_missing_heatmap`: Missing value heatmap
#' - `qc_missing_samples_bar`: Missing value bar plot on samples
#' - `qc_missing_variables_bar`: Missing value bar plot on variables
#' - `qc_tic_bar`: Total intensity count bar plot
#' - `qc_rank_abundance`: Rank abundance plot
#' - `qc_int_boxplot`: Intensity boxplot
#' - `qc_rle`: RLE plot
#' - `qc_cv_dent`: CV density plot
#' - `qc_batch_pca`: PCA score plot colored by batch (if `batch_col` provided)
#' - `qc_rep_scatter`: Replicate scatter plots (if `rep_col` provided)
#'
#' When `when = "pre"`, plots are prefixed with `qc_pre_` to distinguish from post-QC plots.
#' When `when = "post"` or `NULL`, plots use the standard `qc_` prefix.
#'
#' @param when Character string indicating when this QC step is run.
#'   Use `"pre"` for pre-preprocessing QC, `"post"` for post-preprocessing QC,
#'   or `"generic"` (also `NULL`) for generic QC. This is appended to the step ID.
#'   Default is `"post"`.
#' @param batch_col Column name for batch information (for `glyclean::plot_batch_pca()`).
#' @param rep_col Column name for replicate information (for `glyclean::plot_rep_scatter()`).
#' @param plot_width Width of plots in inches. Default is 7.
#' @param plot_height Height of plots in inches. Default is 5.
#' @param method A single QC plot to generate at any stage. Choose
#'   `"missing_heatmap"`, `"missing_bar"`, `"tic_bar"`, `"rank_abundance"`,
#'   `"int_boxplot"`, `"rle"`, `"cv_dent"`, `"batch_pca"` or `"rep_scatter"`.
#' @param ... Arguments for the selected glyclean plot, such as `on = "variable"`
#'   for `"missing_bar"`, `by` for `"rle"`, or `n_pairs` and `seed` for
#'   `"rep_scatter"`. An individual plot uses its glyclean defaults.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_plot_qc(when = "pre")
#' step_plot_qc(when = "post")
#' step_plot_qc(when = "post", method = "missing_bar", on = "variable")
#' @seealso [glyclean::plot_missing_heatmap()], [glyclean::plot_tic_bar()], and other glyclean plotting functions.
#' @export
step_plot_qc <- function(
  when = "post",
  batch_col = "batch",
  rep_col = NULL,
  plot_width = 7,
  plot_height = 5,
  method = NULL,
  ...
) {
  checkmate::assert_choice(when, c("pre", "post", "generic"), null.ok = TRUE)
  when <- when %||% "generic"
  plots <- NULL
  args <- rlang::list2(...)
  label <- switch(
    when,
    pre = "QC (pre-preprocessing)",
    post = "QC (post-preprocessing)",
    generic = "QC"
  )
  id <- paste0("plot_qc_", when)
  if (!is.null(method)) {
    method <- rlang::arg_match(method, names(.qc_plot_descriptions))
    if (method == "batch_pca") {
      args$batch_col <- batch_col
    }
    if (method == "rep_scatter") {
      args$rep_col <- rep_col
    }
    plots <- rlang::set_names(list(args), method)
    id <- paste0(id, "_", .qc_plot_name(method, args))
    label <- paste(label, method)
  } else if (length(args)) {
    cli::cli_abort(
      "Select a QC {.arg method} to supply plot-specific arguments."
    )
  }

  step(
    id = id,
    connected = .exp_port(),
    label = label,
    repeatable = TRUE,
    run = function(ctx) {
      .run_plot_qc(
        ctx,
        when,
        batch_col,
        rep_col,
        plot_width,
        plot_height,
        plots
      )
    },
    require = "exp",
    packages = c(
      "glyclean",
      switch(
        method %||% "",
        missing_heatmap = c("pheatmap", "ggplotify"),
        batch_pca = "factoextra",
        rep_scatter = "patchwork"
      )
    ),
    signature = rlang::expr_deparse(match.call())
  )
}

.run_plot_qc <- function(
  ctx,
  when,
  batch_col,
  rep_col,
  plot_width,
  plot_height,
  plots = NULL
) {
  exp <- ctx_get_data(ctx, "exp")
  plan <- plots %||% .default_qc_plots(exp, when, batch_col, rep_col)
  for (i in seq_along(plan)) {
    method <- names(plan)[i]
    args <- plan[[i]]
    name <- .qc_plot_name(method, args)
    plot_id <- paste0(if (when == "pre") "qc_pre_" else "qc_", name)
    plot <- tryCatch(
      rlang::exec(
        getExportedValue("glyclean", paste0("plot_", method)),
        exp = exp,
        !!!args
      ),
      error = function(e) {
        if (!is.null(plots)) {
          stop(e)
        }
        cli::cli_warn(
          "Failed to generate plot {plot_id}: {conditionMessage(e)}"
        )
        NULL
      }
    )
    if (!is.null(plot)) {
      desc <- switch(
        name,
        missing_samples_bar = "Missing value bar plot on samples.",
        missing_variables_bar = "Missing value bar plot on variables.",
        .qc_plot_descriptions[[method]]
      )
      if (when == "pre") {
        desc <- paste0("Preprocess QC (pre): ", desc)
      }
      ctx <- ctx_add_plot(
        ctx,
        plot_id,
        plot,
        desc,
        width = plot_width,
        height = plot_height
      )
    }
  }
  ctx
}

.qc_plot_descriptions <- c(
  missing_heatmap = "Missing value heatmap.",
  missing_bar = "Missing value proportions.",
  tic_bar = "Total intensity count (TIC) bar plot.",
  rank_abundance = "Rank abundance plot.",
  int_boxplot = "Log2 intensity boxplot.",
  rle = "Relative Log Expression (RLE) plot.",
  cv_dent = "Coefficient of Variation (CV) density plot.",
  batch_pca = "PCA score plot colored by batch.",
  rep_scatter = "Replicate scatter plots."
)

.qc_plot_name <- function(method, args) {
  if (method == "missing_bar") {
    on <- rlang::arg_match0(
      args$on %||% "sample",
      c("sample", "samples", "variable", "variables")
    )
    return(
      if (on %in% c("sample", "samples")) {
        "missing_samples_bar"
      } else {
        "missing_variables_bar"
      }
    )
  }
  method
}

.default_qc_plots <- function(exp, when, batch_col, rep_col) {
  plots <- list()
  if (when == "pre") {
    plots <- list(
      missing_heatmap = list(),
      missing_bar = list(on = "sample"),
      missing_bar = list(on = "variable")
    )
  }
  plots <- c(
    plots,
    list(
      tic_bar = list(),
      rank_abundance = list(),
      int_boxplot = list(by = "group"),
      rle = list(by = "group"),
      cv_dent = list(by = "group")
    )
  )
  sample_cols <- colnames(.get_sample_info(exp))
  if (!is.null(batch_col) && batch_col %in% sample_cols) {
    plots$batch_pca <- list(batch_col = batch_col)
  }
  if (!is.null(rep_col) && rep_col %in% sample_cols) {
    plots$rep_scatter <- list(rep_col = rep_col)
  }
  plots
}

#' Step: Preprocessing
#'
#' @description
#' Preprocess the experiment using `glyclean::auto_clean()`.
#' This step can be omitted if the experiment is already preprocessed.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment to preprocess
#'
#' Data generated:
#' - `raw_exp`: The input to the first preprocessing step
#'
#' Subsequent steps read the processed experiment from `exp`.
#'
#' @param batch_col Column name for batch information (for QC plots and batch effect handling).
#' @param normalize_to_try Legacy argument for versions of `auto_clean()` that
#'   accept candidate methods. Use [step_normalize()] to select a method.
#' @param impute_to_try Legacy argument for versions of `auto_clean()` that
#'   accept candidate methods. Use [step_impute()] to select a method.
#' @param remove_preset Preset for data removal: "discovery", "biomarker", or NULL.
#' @param batch_prop_threshold Threshold for batch proportion filtering.
#' @param check_batch_confounding Whether to check for batch confounding.
#' @param batch_confounding_threshold Threshold for batch confounding detection.
#' @param rep_col Column name for replicate information (for QC plots).
#' @inheritParams glyclean::auto_clean
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_preprocess()
#' step_preprocess(remove_preset = "discovery")
#' @seealso [glyclean::auto_clean()], [step_remove()], [step_normalize()],
#'   [step_impute()], [step_aggregate()], [step_correct_batch_effect()]
#' @export
step_preprocess <- function(
  batch_col = "batch",
  normalize_to_try = NULL,
  impute_to_try = NULL,
  remove_preset = "discovery",
  batch_prop_threshold = 0.3,
  check_batch_confounding = TRUE,
  batch_confounding_threshold = 0.4,
  rep_col = NULL,
  group_col = "group",
  standardize_variable = TRUE,
  seed = 123
) {
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "preprocess",
    connected = .exp_port("exp"),
    label = "Preprocessing",
    run = function(ctx) {
      .run_preprocess(
        ctx,
        batch_col = batch_col,
        normalize_to_try = normalize_to_try,
        impute_to_try = impute_to_try,
        remove_preset = remove_preset,
        batch_prop_threshold = batch_prop_threshold,
        check_batch_confounding = check_batch_confounding,
        batch_confounding_threshold = batch_confounding_threshold,
        group_col = group_col,
        standardize_variable = standardize_variable,
        seed = seed
      )
    },
    report = function(x) {
      .report_preprocess(x)
    },
    require = "exp",
    initialize = "raw_exp",
    packages = "glyclean",
    signature = signature
  )
}

#' Run preprocessing
#'
#' @param ctx Analysis context.
#' @inheritParams step_preprocess
#'
#' @returns Updated analysis context.
#' @noRd
.run_preprocess <- function(
  ctx,
  batch_col,
  normalize_to_try,
  impute_to_try,
  remove_preset,
  batch_prop_threshold,
  check_batch_confounding,
  batch_confounding_threshold,
  group_col = "group",
  standardize_variable = TRUE,
  seed = 123
) {
  exp <- ctx_get_data(ctx, "exp")
  clean_args <- list(
    batch_col = batch_col,
    remove_preset = remove_preset,
    batch_prop_threshold = batch_prop_threshold,
    check_batch_confounding = check_batch_confounding,
    batch_confounding_threshold = batch_confounding_threshold,
    group_col = group_col,
    standardize_variable = standardize_variable,
    seed = seed
  )
  supported_args <- names(formals(glyclean::auto_clean))
  if ("normalize_to_try" %in% supported_args) {
    clean_args[["normalize_to_try"]] <- normalize_to_try
  }
  if ("impute_to_try" %in% supported_args) {
    clean_args[["impute_to_try"]] <- impute_to_try
  }
  clean_exp <- rlang::exec(glyclean::auto_clean, exp, !!!clean_args)
  .store_preprocessed_exp(ctx, clean_exp)
}

#' Report preprocessing results
#'
#' @param x Analysis context after preprocessing.
#'
#' @returns A summary string for the report.
#' @noRd
.report_preprocess <- function(x) {
  text <- paste(x$meta$logs$preprocess, collapse = "\n")
  replacements <- c(
    "gfs" = "glycoform (with structure)",
    "gf" = "glycoform",
    "gps" = "glycopeptide (with structure)",
    "gp" = "glycopeptide"
  )
  for (pattern in names(replacements)) {
    text <- stringr::str_replace_all(text, pattern, replacements[[pattern]])
  }
  paste0("<AI>", text, "</AI>")
}

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
    connected = .exp_port(),
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
  packages = character(),
  call = rlang::caller_call()
) {
  force(args)
  step(
    id = id,
    connected = .exp_port("exp"),
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
      result <- rlang::exec(fun, !!!input, !!!args)
      .store_preprocessed_exp(ctx, result)
    },
    require = "exp",
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

#' Compositional data transformation
#'
#' Transform the selected experiment to centered log-ratio (CLR) or additive
#' log-ratio (ALR) coordinates. By default, [glyclean::auto_coda()] chooses
#' the transformation.
#'
#' @details
#' Place this step after abundance preprocessing and before statistical
#' analysis. The input must contain finite, positive abundances on the raw
#' scale or finite values on a known log scale. A transformation error stops
#' the blueprint so subsequent tests cannot accidentally use the input scale.
#'
#' The step replaces `on` with the transformed experiment and saves its
#' previous value in `pre_coda_<on>` (by default, `pre_coda_exp`). Subsequent
#' steps targeting the same experiment use its log2-ratio coordinates
#' directly. A successful ALR omits the reference feature from those tests.
#' Use [glyclean::get_pipeline()] on the output to see which transform ran.
#'
#' Differential tests return p-values on these coordinates. For example,
#' limma's `estimate` is the contrast in log2-ratio units. Abundance
#' `log2fc` is undefined, so significant features are selected by p-value
#' ([glystats::filter_sig_vars()] ignores an explicit fold-change cutoff
#' with a warning). Volcano plots and fold-change-based protein enrichment
#' are skipped. Run abundance QC, trait derivation, and motif
#' quantification before transforming the main experiment.
#'
#' @inheritParams glyclean::auto_coda
#' @param method Use `"auto"` (default) for [glyclean::auto_coda()], `"clr"`
#'   for [glyclean::transform_clr()], or `"alr"` for [glyclean::transform_alr()].
#'   Automatic selection uses site-wise CLR for glycoproteomics. For other
#'   experiments, it uses CLR for up to 50 features and attempts ALR above 50.
#'   The ALR method falls back to CLR if no suitable reference is available.
#'   Glycoproteomics supports `"auto"` and `"clr"`.
#' @param on Name of the experiment in the blueprint. Defaults to `"exp"`.
#'   For example, use `"dynamic_motif_exp"` or `"branch_motif_exp"` after
#'   quantifying motifs, then use the same `on` in the statistical step.
#' @returns A `glysmith_step` object.
#' @examples
#' blueprint(step_coda(by = "group", gamma = 0), step_pca(), step_dea_limma())
#' step_coda(method = "clr", gamma = 0)
#' blueprint(
#'   step_quantify_branch_motifs(),
#'   step_coda(on = "branch_motif_exp"),
#'   step_dea_limma(on = "branch_motif_exp")
#' )
#' @export
step_coda <- function(
  method = "auto",
  by = NULL,
  gamma = 0.1,
  group_scales = NULL,
  seed = 123,
  on = "exp"
) {
  signature <- rlang::expr_deparse(match.call())
  method <- rlang::arg_match(method, c("auto", "clr", "alr"))
  on_meta <- .resolve_on(on)
  backup <- paste0("pre_coda_", on)
  args <- list(by = by, gamma = gamma, group_scales = group_scales, seed = seed)

  step(
    id = paste0("coda", on_meta$id_suffix),
    connected = .exp_port("exp"),
    label = paste0("Compositional data transformation", on_meta$label_suffix),
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      transform <- switch(
        method,
        auto = glyclean::auto_coda,
        clr = glyclean::transform_clr,
        alr = glyclean::transform_alr
      )
      transformed <- rlang::exec(transform, exp, !!!args)
      ctx <- ctx_add_data(ctx, backup, exp)
      ctx_add_data(ctx, on, transformed)
    },
    report = function(x) {
      paste0(
        "The `",
        on,
        "` experiment was transformed to log2-ratio coordinates."
      )
    },
    require = on,
    generate = backup,
    packages = "glyclean",
    stop_on_error = TRUE,
    signature = signature
  )
}

#' Step: Subset Groups
#'
#' @description
#' Subset the experiment to specific groups using the `group` column in sample information.
#' This is useful when downstream steps require exactly two groups for comparison.
#' Usually run after `step_preprocess()` and before DEA or enrichment steps.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment to subset
#'
#' Data generated:
#' - `full_exp`: The original experiment before subsetting
#'
#' This step overwrites `exp` in the context with the subset experiment.
#'
#' @param groups Group names to keep. If `NULL`, this step will be skipped.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_subset_groups(groups = c("H", "C"))
#' @export
step_subset_groups <- function(groups = NULL) {
  checkmate::assert_character(
    groups,
    min.len = 1,
    unique = TRUE,
    null.ok = TRUE
  )
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "subset_groups",
    connected = .exp_port("exp"),
    label = "Subset groups",
    condition = function(ctx) {
      .condition_subset_groups(ctx, groups = groups)
    },
    run = function(ctx) {
      .run_subset_groups(ctx, groups = groups)
    },
    require = "exp",
    generate = "full_exp",
    signature = signature
  )
}

#' Check whether group subsetting should run
#'
#' @param ctx Analysis context.
#' @param groups Group names to keep.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_subset_groups <- function(ctx, groups) {
  if (is.null(groups)) {
    return(list(check = FALSE, reason = "group subset is not provided"))
  }
  list(check = TRUE, reason = NULL)
}

#' Run group subsetting
#'
#' @param ctx Analysis context.
#' @param groups Group names to keep.
#'
#' @returns Updated analysis context.
#' @noRd
.run_subset_groups <- function(ctx, groups) {
  exp <- ctx_get_data(ctx, "exp")
  sample_info <- .get_sample_info(exp)
  available <- unique(as.character(sample_info[["group"]]))
  missing <- setdiff(groups, available)
  if (length(missing) > 0) {
    missing_txt <- paste(missing, collapse = ", ")
    cli::cli_abort("Group(s) not found in experiment: {missing_txt}.")
  }

  full_exp <- exp
  exp <- .filter_groups(exp, groups)

  ctx <- ctx_add_data(ctx, "exp", exp)
  ctx_add_data(ctx, "full_exp", full_exp)
}

#' Read a protein expression matrix
#'
#' @param path Path to a `.csv`, `.tsv`, or `.rds` protein expression matrix.
#'
#' @returns A numeric matrix with protein accessions as row names.
#' @noRd
.read_pro_expr_mat <- function(path) {
  checkmate::assert_character(path, len = 1)
  if (!fs::file_exists(path)) {
    cli::cli_abort("Protein expression matrix file not found: {.val {path}}.")
  }

  ext <- stringr::str_to_lower(fs::path_ext(path))
  if (ext %in% c("csv", "tsv")) {
    reader <- if (ext == "csv") readr::read_csv else readr::read_tsv
    tbl <- reader(path, show_col_types = FALSE)
    if (ncol(tbl) < 2) {
      cli::cli_abort(
        "Protein expression file must have at least two columns (protein + samples)."
      )
    }
    protein <- tbl[[1]]
    if (any(is.na(protein) | !nzchar(protein))) {
      cli::cli_abort(
        "Protein accessions in the first column must be non-empty."
      )
    }
    if (anyDuplicated(protein) > 0) {
      cli::cli_abort("Protein accessions in the first column must be unique.")
    }
    tbl <- tbl[, -1, drop = FALSE]
    mat <- as.matrix(tbl)
    rownames(mat) <- protein
    return(mat)
  }

  if (ext == "rds") {
    obj <- readRDS(path)
    if (is.data.frame(obj)) {
      if (
        is.null(rownames(obj)) ||
          any(is.na(rownames(obj)) | !nzchar(rownames(obj)))
      ) {
        cli::cli_abort(
          "RDS data.frame must have non-empty row names (protein accessions)."
        )
      }
      obj <- as.matrix(obj)
    }
    if (!is.matrix(obj)) {
      cli::cli_abort(
        "RDS file must contain a matrix or data.frame with row names."
      )
    }
    if (
      is.null(rownames(obj)) ||
        any(is.na(rownames(obj)) | !nzchar(rownames(obj)))
    ) {
      cli::cli_abort("Protein accessions in row names must be non-empty.")
    }
    if (anyDuplicated(rownames(obj)) > 0) {
      cli::cli_abort("Protein accessions in row names must be unique.")
    }
    return(obj)
  }

  cli::cli_abort(
    "Unsupported protein expression file extension: {.val {ext}}. Use .csv, .tsv, or .rds."
  )
}

#' Step: Adjust Protein Abundance
#'
#' @description
#' Adjust glycoform quantification values by correcting for protein abundance
#' utilizing `glyclean::adjust_protein()`.
#' Usually this step should be run after `step_preprocess()`.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment to adjust
#'
#' Data generated:
#' - `unadj_exp`: The original experiment (previous `exp`, saved for reference)
#'
#' This step is special in that it silently overwrites the `exp` data with the adjusted experiment.
#' This ensures that no matter if adjustment is performed or not,
#' the "active" experiment is always under the key `exp`.
#' The previous `exp` is saved as `unadj_exp` for reference.
#'
#' @param pro_expr_path Path to the protein expression matrix file.
#'   If `NULL`, this step will be skipped.
#'   Can be:
#'   - A CSV/TSV file with the first column as protein accessions and remaining columns as sample names.
#'   - An RDS file with a matrix or data.frame with row names as protein accessions and columns as sample names.
#' @param pro_scale Representation of the protein matrix: `"raw"` (default),
#'   `"log"` (natural logarithm), `"log2"`, or `"log10"`. This is independent
#'   of the experiment scale. Supply the scale actually stored in the file;
#'   no scale is inferred. Ordinary logarithms are supported, not log(x + 1).
#'   The adjusted experiment retains its original scale. `"logx"` and unknown
#'   scales are rejected because they cannot be decoded unambiguously.
#' @inheritParams glyclean::adjust_protein
#'
#' @return A `glysmith_step` object.
#' @examples
#' fake_pro_expr_mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' rownames(fake_pro_expr_mat) <- paste0("P", seq_len(10))
#' colnames(fake_pro_expr_mat) <- paste0("S", seq_len(10))
#' fake_pro_expr_path <- tempfile(fileext = ".rds")
#' saveRDS(fake_pro_expr_mat, fake_pro_expr_path)
#' step_adjust_protein(fake_pro_expr_path)
#'
#' @seealso [glyclean::adjust_protein()]
#' @export
step_adjust_protein <- function(
  pro_expr_path = NULL,
  method = "ratio",
  pro_scale = "raw"
) {
  if (!is.null(pro_expr_path)) {
    checkmate::assert_file_exists(
      pro_expr_path,
      access = "r",
      extension = c("csv", "tsv", "rds")
    )
  }
  checkmate::assert_choice(method, c("ratio", "reg"))
  checkmate::assert_choice(pro_scale, c("raw", "log", "log2", "log10"))
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "adjust_protein",
    connected = .exp_port("exp"),
    label = "Protein adjustment",
    condition = function(ctx) {
      .condition_adjust_protein(ctx, pro_expr_path = pro_expr_path)
    },
    run = function(ctx) {
      .run_adjust_protein(
        ctx,
        pro_expr_path = pro_expr_path,
        method = method,
        pro_scale = pro_scale
      )
    },
    report = function(x) {
      .report_adjust_protein(x)
    },
    require = "exp",
    generate = "unadj_exp",
    packages = "glyclean",
    signature = signature
  )
}

#' Check whether protein adjustment should run
#'
#' @param ctx Analysis context.
#' @param pro_expr_path Path to the protein expression matrix file.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_adjust_protein <- function(ctx, pro_expr_path) {
  if (is.null(pro_expr_path)) {
    return(list(
      check = FALSE,
      reason = "protein expression path is not provided"
    ))
  }
  if (.get_exp_type(ctx_get_data(ctx, "exp")) != "glycoproteomics") {
    return(list(
      check = FALSE,
      reason = "input is not a glycoproteomics experiment"
    ))
  }
  list(check = TRUE, reason = NULL)
}

#' Run protein abundance adjustment
#'
#' @param ctx Analysis context.
#' @inheritParams step_adjust_protein
#'
#' @returns Updated analysis context.
#' @noRd
.run_adjust_protein <- function(ctx, pro_expr_path, method, pro_scale = "raw") {
  exp <- ctx_get_data(ctx, "exp")
  pro_expr_mat <- .read_pro_expr_mat(pro_expr_path)
  adj_exp <- glyclean::adjust_protein(
    exp,
    pro_expr_mat,
    method = method,
    pro_scale = pro_scale
  )
  ctx <- ctx_add_data(ctx, "exp", adj_exp)
  ctx_add_data(ctx, "unadj_exp", exp)
}

#' Report protein abundance adjustment results
#'
#' @param x Analysis context after protein adjustment.
#'
#' @returns A summary string for the report.
#' @noRd
.report_adjust_protein <- function(x) {
  logs <- x$meta$logs$adjust_protein %||% list()
  msg_lines <- logs$message %||% character(0)
  if (length(msg_lines) == 0) {
    msg_lines <- logs$output %||% character(0)
  }
  base <- "Protein-adjusted glycoform quantification was performed."
  if (length(msg_lines) == 0) {
    return(base)
  }
  paste(c(base, msg_lines), collapse = "\n")
}

#' Step: Identification Overview
#'
#' @description
#' Summarize the experiment using `glyexp::summarize_experiment()`.
#' This is usually the first step, BEFORE `step_preprocess()`.
#' Very light-weight to run, so always include it.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment to summarize
#'
#' Tables generated:
#' - `summary`: A table containing the identification overview of the experiment
#'
#' @inheritParams glyexp::summarize_experiment
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_ident_overview()
#' @seealso [glyexp::summarize_experiment()]
#' @export
step_ident_overview <- function(count_struct = NULL) {
  signature <- rlang::expr_deparse(match.call())
  step(
    id = "ident_overview",
    connected = .exp_port(),
    label = "Identification overview",
    run = function(ctx) {
      .run_ident_overview(ctx, count_struct = count_struct)
    },
    report = function(x) {
      .report_ident_overview(x)
    },
    require = "exp",
    signature = signature
  )
}

#' Run identification overview
#'
#' @param ctx Analysis context.
#' @inheritParams step_ident_overview
#'
#' @returns Updated analysis context.
#' @noRd
.run_ident_overview <- function(ctx, count_struct) {
  exp <- ctx_get_data(ctx, "exp")
  tbl <- glyexp::summarize_experiment(exp, count_struct = count_struct)
  ctx_add_table(
    ctx,
    "summary",
    tbl,
    "Identification overview of the experiment."
  )
}

#' Report identification overview results
#'
#' @param x Analysis context after identification overview.
#'
#' @returns A summary string for the report.
#' @noRd
.report_ident_overview <- function(x) {
  tbl <- x$tables[["summary"]]
  total_tbl <- dplyr::filter(tbl, stringr::str_starts(.data$item, "total_"))
  total_parts <- paste0(
    total_tbl$n,
    " ",
    stringr::str_remove(total_tbl$item, "total_"),
    "s"
  )
  sample_tbl <- dplyr::filter(
    tbl,
    stringr::str_ends(.data$item, "_per_sample")
  )
  sample_parts <- paste0(
    sample_tbl$n,
    " ",
    stringr::str_remove(sample_tbl$item, "_per_sample"),
    "s"
  )
  glue::glue(
    "In total, there are {paste(total_parts, collapse = ', ')}. On average, there are {paste(sample_parts, collapse = ', ')} per sample."
  )
}
