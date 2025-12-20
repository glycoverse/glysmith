#' Create a GlySmith step object
#'
#' A step is a function object with metadata that mutates and returns context.
#'
#' @param id Step id.
#' @param label Human-readable label for progress output.
#' @param run A function(ctx) that returns updated ctx.
#' @param report A function(x) that returns a markdown string for reporting, or NULL.
#'   The string can contain additional information surrounded by "<AI>" and "</AI>" tags,
#'   which will be fed to AI for polishing.
#'   If AI polishing is turned off, the content in "<AI>" and "</AI>" tags will be removed.
#' @param require Character vector of required `ctx$data` keys.
#' @param generate Character vector of generated `ctx$data` keys.
#' @param condition Optional function(ctx) returning a list of `check` and `reason`.
#'   - `check` is TRUE/FALSE to decide execution.
#'   - `reason` is a human-readable string to explain why `check` is FALSE.
#' @param signature The original call signature for display in messages.
#'
#' @returns A `glysmith_step` object.
#' @noRd
step <- function(
  id,
  label,
  run,
  report = NULL,
  outputs = list(),
  require = character(0),
  generate = character(0),
  condition = NULL,
  retry = 0L,
  signature = NULL
) {
  structure(
    list(
      id = id,
      label = label,
      run = run,
      report = report,
      require = require,
      generate = generate,
      condition = condition,
      retry = retry,
      signature = signature %||% paste0("step_", id, "()")
    ),
    class = "glysmith_step"
  )
}

#' @export
print.glysmith_step <- function(x, ...) {
  cli::cli_text("<step {.val {x$signature}}> {.emph {x$label}}")
}

#' All steps in GlySmith
#'
#' @return A list of `glysmith_step` objects.
#' @noRd
all_steps <- function() {
  steps <- list(
    step_preprocess(),
    step_ident_overview(),
    step_pca(),
    step_heatmap(),
    step_dea_limma(),
    step_dea_ttest(),
    step_dea_wilcox(),
    step_dea_anova(),
    step_dea_kruskal(),
    step_volcano(),
    step_sig_enrich_go(),
    step_sig_enrich_kegg(),
    step_sig_enrich_reactome(),
    step_derive_traits()
  )
  names(steps) <- purrr::map_chr(steps, "id")
  steps
}

#' Step: Preprocessing
#'
#' Preprocess the experiment using `glyclean::auto_clean()`.
#' This is usually the first step, but can be omitted if the experiment is already preprocessed.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to preprocess
#'
#' Data generated:
#' - `raw_exp`: The raw experiment (previous `exp`, saved for reference)
#'
#' This step is special in that it silently overwrites the `exp` data with the preprocessed experiment.
#' This ensures that no matter if preprocessing is performed or not,
#' the "active" experiment is always under the key `exp`.
#' The previous `exp` is saved as `raw_exp` for reference.
#'
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_preprocess(glyclean.auto_clean.remove_preset = "discovery")`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_preprocess()
#' @seealso [glyclean::auto_clean()]
#' @export
step_preprocess <- function(...) {
  signature <- rlang::expr_deparse(match.call())
  step_dots <- rlang::list2(...)
  step(
    id = "preprocess",
    label = "Preprocessing",
    run = function(ctx) {
      # Here we use a bit of a hack to overwrite the exp data with the preprocessed exp.
      # This ensures the "active" experiment is always under the key "exp",
      # no matter if preprocessing is performed or not.
      exp <- ctx_get_data(ctx, "exp")
      clean_exp <- .run_function(
        glyclean::auto_clean,
        exp,
        step_id = "preprocess",
        global_dots = ctx$dots,
        step_dots = step_dots
      )
      ctx <- ctx_add_data(ctx, "exp", clean_exp)  # overwrite exp with preprocessed exp
      ctx <- ctx_add_data(ctx, "raw_exp", exp)  # keep raw exp for reference
      ctx
    },
    report = function(x) {
      text <- paste(x$meta$logs$preprocess, collapse = "\n")
      text <- stringr::str_replace_all(text, "gf", "glycoform")
      text <- stringr::str_replace_all(text, "gfs", "glycoform (with structure)")
      text <- stringr::str_replace_all(text, "gp", "glycopeptide")
      text <- stringr::str_replace_all(text, "gps", "glycopeptide (with structure)")
      paste0("<AI>", text, "</AI>")
    },
    require = "exp",
    generate = "raw_exp",
    signature = signature
  )
}

#' Step: Identification Overview
#'
#' Summarize the experiment using `glyexp::summarize_experiment()`.
#' This step can be run at any time, but is usually run before or right after preprocessing.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to summarize
#'
#' Tables generated:
#' - `summary`: A table containing the identification overview of the experiment
#'
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_ident_overview(glyexp.summarize_experiment.count_struct = FALSE)`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_ident_overview()
#' @seealso [glyexp::summarize_experiment()]
#' @export
step_ident_overview <- function(...) {
  signature <- rlang::expr_deparse(match.call())
  step_dots <- rlang::list2(...)
  step(
    id = "ident_overview",
    label = "Identification overview",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      tbl <- .run_function(
        glyexp::summarize_experiment,
        exp,
        step_id = "ident_overview",
        global_dots = ctx$dots,
        step_dots = step_dots
      )
      ctx_add_table(ctx, "summary", tbl, "Identification overview of the experiment.")
    },
    report = function(x) {
      tbl <- x$tables[["summary"]]
      parts <- paste0(tbl$n, " ", tbl$item, "s")
      paste0(
        "In total, ",
        glue::glue_collapse(parts, sep = ", ", last = ", and "),
        " were identified."
      )
    },
    require = "exp",
    signature = signature
  )
}

#' Step: Principal Component Analysis (PCA)
#'
#' Run PCA using `glystats::gly_pca()` and plot it with `glyvis::plot_pca()`.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to run PCA on
#'
#' Tables generated:
#' - `pca_samples`: A table containing the PCA scores for each sample
#' - `pca_variables`: A table containing the PCA loadings for each variable
#' - `pca_eigenvalues`: A table containing the PCA eigenvalues
#'
#' Plots generated:
#' - `pca`: A PCA plot colored by group
#'
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_pca(glystats.gly_pca.center = FALSE)`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_pca()
#' @seealso [glystats::gly_pca()], [glyvis::plot_pca()]
#' @export
step_pca <- function(...) {
  signature <- rlang::expr_deparse(match.call())
  step_dots <- rlang::list2(...)
  step(
    id = "pca",
    label = "Principal component analysis",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      pca_res <- .run_function(
        glystats::gly_pca,
        exp,
        step_id = "pca",
        global_dots = ctx$dots,
        step_dots = step_dots
      )
      ctx <- ctx_add_table(
        ctx,
        "pca_samples",
        glystats::get_tidy_result(pca_res, "samples"),
        "PCA scores for each sample."
      )
      ctx <- ctx_add_table(
        ctx,
        "pca_variables",
        glystats::get_tidy_result(pca_res, "variables"),
        "PCA loadings for each variable."
      )
      ctx <- ctx_add_table(
        ctx,
        "pca_eigenvalues",
        glystats::get_tidy_result(pca_res, "eigenvalues"),
        "PCA eigenvalues."
      )
      p <- .run_function(
        glyvis::plot_pca,
        pca_res,
        step_id = "pca",
        global_dots = ctx$dots,
        step_dots = step_dots
      )
      ctx_add_plot(ctx, "pca", p, "PCA plot colored by group.")
    },
    require = "exp",
    signature = signature
  )
}

#' Step: Differential Expression Analysis (DEA) using Limma
#'
#' Run differential analysis using linear model-based analysis via `glystats::gly_limma()`,
#' then filter the experiment to keep only the differentially expressed variables using `glystats::filter_sig_vars()`.
#' By default, this runs DEA on the main experiment (`exp`), but can be configured
#' to run on derived traits (`trait_exp`) or other experiment objects.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Data generated:
#' - `dea_res`: The DEA results (if `on = "exp"`, default)
#' - `dta_res`: The DTA results (if `on = "trait_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#'
#' Tables generated:
#' - `dea`: A table containing the DEA result (if `on = "exp"`, default)
#' - `dta`: A table containing the DTA result (if `on = "trait_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#' @param ... Step-specific arguments passed to `glystats::gly_limma()`.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_dea_limma(glystats.gly_limma.p_adj_method = "BH")`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_limma()
#' step_dea_limma(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_limma()]
#' @export
step_dea_limma <- function(on = "exp", ...) {
  signature <- rlang::expr_deparse(match.call())
  .step_dea(method = "limma", label = "Differential expression analysis (limma)", on = on, signature = signature, ...)
}

#' Step: Differential Expression Analysis (DEA) using t-test
#'
#' Run differential analysis using t-test via `glystats::gly_ttest()`,
#' then filter the experiment to keep only the differentially expressed variables using `glystats::filter_sig_vars()`.
#' By default, this runs DEA on the main experiment (`exp`), but can be configured
#' to run on derived traits (`trait_exp`) or other experiment objects.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Data generated:
#' - `dea_res`: The DEA results (if `on = "exp"`, default)
#' - `dta_res`: The DTA results (if `on = "trait_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#'
#' Tables generated:
#' - `dea`: A table containing the DEA result (if `on = "exp"`, default)
#' - `dta`: A table containing the DTA result (if `on = "trait_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#' @param ... Step-specific arguments passed to `glystats::gly_ttest()`.
#'   Use the format `pkg.func.arg`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_ttest()
#' step_dea_ttest(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_ttest()]
#' @export
step_dea_ttest <- function(on = "exp", ...) {
  signature <- rlang::expr_deparse(match.call())
  .step_dea(method = "ttest", label = "Differential expression analysis (t-test)", on = on, signature = signature, ...)
}

#' Step: Differential Expression Analysis (DEA) using ANOVA
#'
#' Run differential analysis using ANOVA via `glystats::gly_anova()`,
#' then filter the experiment to keep only the differentially expressed variables using `glystats::filter_sig_vars()`.
#' By default, this runs DEA on the main experiment (`exp`), but can be configured
#' to run on derived traits (`trait_exp`) or other experiment objects.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Data generated:
#' - `dea_res`: The DEA results (if `on = "exp"`, default)
#' - `dta_res`: The DTA results (if `on = "trait_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#'
#' Tables generated:
#' - `dea_main_test`, `dea_post_hoc_test`: Tables containing the results (if `on = "exp"`, default)
#' - `dta_main_test`, `dta_post_hoc_test`: Tables containing the results (if `on = "trait_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#' @param ... Step-specific arguments passed to `glystats::gly_anova()`.
#'   Use the format `pkg.func.arg`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_anova()
#' step_dea_anova(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_anova()]
#' @export
step_dea_anova <- function(on = "exp", ...) {
  signature <- rlang::expr_deparse(match.call())
  .step_dea(method = "anova", label = "Differential expression analysis (ANOVA)", on = on, signature = signature, ...)
}

#' Step: Differential Expression Analysis (DEA) using Wilcoxon test
#'
#' Run differential analysis using Wilcoxon analysis via `glystats::gly_wilcox()`,
#' then filter the experiment to keep only the differentially expressed variables using `glystats::filter_sig_vars()`.
#' By default, this runs DEA on the main experiment (`exp`), but can be configured
#' to run on derived traits (`trait_exp`) or other experiment objects.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Data generated:
#' - `dea_res`: The DEA results (if `on = "exp"`, default)
#' - `dta_res`: The DTA results (if `on = "trait_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#'
#' Tables generated:
#' - `dea`: A table containing the DEA result (if `on = "exp"`, default)
#' - `dta`: A table containing the DTA result (if `on = "trait_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#' @param ... Step-specific arguments passed to `glystats::gly_wilcox()`.
#'   Use the format `pkg.func.arg`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_wilcox()
#' step_dea_wilcox(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_wilcox()]
#' @export
step_dea_wilcox <- function(on = "exp", ...) {
  signature <- rlang::expr_deparse(match.call())
  .step_dea(method = "wilcox", label = "Differential expression analysis (Wilcoxon)", on = on, signature = signature, ...)
}

#' Step: Differential Expression Analysis (DEA) using Kruskal-Wallis test
#'
#' Run differential analysis using Kruskal-Wallis analysis via `glystats::gly_kruskal()`,
#' then filter the experiment to keep only the differentially expressed variables using `glystats::filter_sig_vars()`.
#' By default, this runs DEA on the main experiment (`exp`), but can be configured
#' to run on derived traits (`trait_exp`) or other experiment objects.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Data generated:
#' - `dea_res`: The DEA results (if `on = "exp"`, default)
#' - `dta_res`: The DTA results (if `on = "trait_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#'
#' Tables generated:
#' - `dea_main_test`, `dea_post_hoc_test`: Tables containing the results (if `on = "exp"`, default)
#' - `dta_main_test`, `dta_post_hoc_test`: Tables containing the results (if `on = "trait_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#' @param ... Step-specific arguments passed to `glystats::gly_kruskal()`.
#'   Use the format `pkg.func.arg`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_kruskal()
#' step_dea_kruskal(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_kruskal()]
#' @export
step_dea_kruskal <- function(on = "exp", ...) {
  signature <- rlang::expr_deparse(match.call())
  .step_dea(method = "kruskal", label = "Differential expression analysis (Kruskal-Wallis)", on = on, signature = signature, ...)
}

#' Get metadata for DEA analysis based on the target experiment
#'
#' @param on Name of the experiment data in ctx$data to run DEA on.
#' @returns A list with prefix, label, require, and name fields.
#' @noRd
.get_dea_meta <- function(on) {
  mapping <- list(
    exp = list(
      prefix = "dea",
      label = "Differential expression",
      require = "exp",
      name = "variable"
    ),
    trait_exp = list(
      prefix = "dta",
      label = "Differential trait",
      require = "trait_exp",
      name = "trait"
    )
  )

  # Return the mapping or create a default one for unknown types
  if (on %in% names(mapping)) {
    mapping[[on]]
  } else {
    list(
      prefix = paste0("d_", on),
      label = paste("Differential", on),
      require = on,
      name = "variable"
    )
  }
}

#' Get the item name for DEA report based on analysis type
#' @param exp Experiment object
#' @param on Target experiment type (exp or trait_exp)
#' @returns A character string for the item name (e.g., "items", "traits")
#' @noRd
.dea_get_item_name <- function(exp, on) {
  if (on == "trait_exp") {
    if (glyexp::get_exp_type(exp) == "glycomics") {
      "traits"
    } else {
      "site-specific traits"
    }
  } else {
    "items"
  }
}

#' Count total significant variables in DEA results
#' @param tbl A tibble with DEA results
#' @returns Number of unique significant variables
#' @noRd
.dea_count_sig_total <- function(tbl) {
  length(unique(tbl$variable[tbl$p_adj < 0.05]))
}

#' Count significant variables per contrast
#' @param tbl A tibble with DEA results (must have ref_group and test_group columns)
#' @returns A named numeric vector with counts per contrast
#' @noRd
.dea_count_sig_per_contrast <- function(tbl) {
  sig_tbl <- tbl[tbl$p_adj < 0.05, ]
  if (nrow(sig_tbl) == 0) {
    return(character(0))
  }
  contrasts <- paste0(sig_tbl$ref_group, " vs ", sig_tbl$test_group)
  table(contrasts)
}

#' Determine if current analysis is multi-group
#' @param tbl A tibble with DEA results
#' @returns TRUE if more than one contrast exists
#' @noRd
.dea_is_multigroup <- function(tbl) {
  if (!"ref_group" %in% colnames(tbl) || !"test_group" %in% colnames(tbl)) {
    return(FALSE)
  }
  n_contrasts <- length(unique(paste0(tbl$ref_group, "_vs_", tbl$test_group)))
  n_contrasts > 1
}

#' Generate DEA report text
#'
#' Creates a report message for differential expression analysis results.
#' For 2-group comparisons, shows only total significant count.
#' For multi-group comparisons, shows total count and per-contrast breakdown.
#'
#' @param x Step result containing tables and exp
#' @param method DEA method (limma, ttest, wilcox, anova, kruskal)
#' @param meta DEA metadata from .get_dea_meta()
#' @param on Target experiment type (exp or trait_exp)
#' @returns A character string for the report
#' @noRd
.dea_report <- function(x, method, meta, on) {
  # Get the appropriate table
  if (method %in% c("anova", "kruskal")) {
    tbl <- x$tables[[paste0(meta$prefix, "_main_test")]]
  } else {
    tbl <- x$tables[[meta$prefix]]
  }

  item_name <- .dea_get_item_name(x$exp, on)
  sig_total <- .dea_count_sig_total(tbl)
  is_multigroup <- .dea_is_multigroup(tbl)

  # Base message

  msg <- paste0(meta$label, " analysis was performed.")

  if (sig_total == 0) {
    msg <- paste0(msg, "No significant ", item_name, " (adjusted p < 0.05).\n")
    return(msg)
  }

  # Add total count
  msg <- paste0(msg, "Number of significant ", item_name, " (adjusted p < 0.05): ", sig_total, ".\n")

  # For multi-group analysis, add per-contrast breakdown
  if (is_multigroup) {
    sig_per_contrast <- .dea_count_sig_per_contrast(tbl)
    if (length(sig_per_contrast) > 0) {
      msg <- paste0(msg, "\nBreakdown by contrast:\n\n")
      for (i in seq_along(sig_per_contrast)) {
        contrast_name <- names(sig_per_contrast)[i]
        contrast_count <- sig_per_contrast[i]
        msg <- paste0(msg, "- ", contrast_name, ": ", contrast_count, "\n")
      }
    }
  }

  msg
}

#' Internal helper for DEA steps
#' @noRd
.step_dea <- function(method, label, on = "exp", signature = NULL, ...) {
  step_dots <- rlang::list2(...)
  meta <- .get_dea_meta(on)

  step(
    id = paste0(meta$prefix, "_", method),
    label = paste0(meta$label, " analysis (", method, ")"),
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      # Apply filtering for trait_exp if needed
      if (on == "trait_exp") {
        exp <- glyclean::remove_constant(exp)
      }

      dea_res <- switch(
        method,
        "limma" = .run_function(glystats::gly_limma, exp, step_id = meta$prefix, global_dots = ctx$dots, step_dots = step_dots),
        "ttest" = .run_function(glystats::gly_ttest, exp, step_id = meta$prefix, global_dots = ctx$dots, step_dots = step_dots),
        "anova" = .run_function(glystats::gly_anova, exp, step_id = meta$prefix, global_dots = ctx$dots, step_dots = step_dots),
        "wilcox" = .run_function(glystats::gly_wilcox, exp, step_id = meta$prefix, global_dots = ctx$dots, step_dots = step_dots),
        "kruskal" = .run_function(glystats::gly_kruskal, exp, step_id = meta$prefix, global_dots = ctx$dots, step_dots = step_dots)
      )
      ctx <- ctx_add_data(ctx, paste0(meta$prefix, "_res"), dea_res)

      if (method %in% c("anova", "kruskal")) {
        ctx <- ctx_add_table(
          ctx,
          paste0(meta$prefix, "_main_test"),
          glystats::get_tidy_result(dea_res, "main_test"),
          paste0("Main test results of ANOVA or Kruskal-Wallis test for ", meta$label, " analysis.")
        )
        ctx <- ctx_add_table(
          ctx,
          paste0(meta$prefix, "_post_hoc_test"),
          glystats::get_tidy_result(dea_res, "post_hoc_test"),
          paste0("Post-hoc test results of ANOVA or Kruskal-Wallis test for ", meta$label, " analysis.")
        )
      } else {
        ctx <- ctx_add_table(
          ctx,
          meta$prefix,
          glystats::get_tidy_result(dea_res),
          paste0(meta$label, " analysis results of all comparisons for all variables.")
        )
      }
      sig_exp <- .run_function(
        glystats::filter_sig_vars,
        exp,
        step_id = meta$prefix,
        global_dots = ctx$dots,
        step_dots = step_dots,
        holy_args = list(res = dea_res)
      )
      ctx <- ctx_add_data(
        ctx,
        paste0("sig_", meta$require),
        sig_exp,
        glue::glue("Experiment with only significant {meta$name}s.")
      )
      ctx
    },
    report = function(x) {
      .dea_report(x, method, meta, on)
    },
    generate = c(paste0(meta$prefix, "_res"), paste0("sig_", meta$require)),
    require = meta$require,
    signature = signature
  )
}

#' Step: Volcano Plot
#'
#' Create a volcano plot from DEA results using `glyvis::plot_volcano()`.
#' This step requires one of the DEA steps to be run:
#' - [step_dea_limma()]
#' - [step_dea_ttest()]
#' - [step_dea_wilcox()]
#'
#' @details
#' Data required:
#' - `dea_res`: The DEA results from `glystats::gly_limma()`
#'
#' Plots generated:
#' - `volcano`: A volcano plot
#'
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_volcano(glyvis.plot_volcano.log2fc_cutoff = 2)`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_volcano()
#' @seealso [glyvis::plot_volcano()]
#' @export
step_volcano <- function(...) {
  signature <- rlang::expr_deparse(match.call())
  step_dots <- rlang::list2(...)
  step(
    id = "volcano",
    label = "Volcano plot",
    condition = function(ctx) {
      check <- !inherits(ctx_get_data(ctx, "dea_res"), "glystats_anova_res") ||
        !inherits(ctx_get_data(ctx, "dea_res"), "glystats_kruskal_res")
      reason <- "volcano plot is not supported for ANOVA or Kruskal-Wallis DEA results."
      list(check = check, reason = reason)
    },
    run = function(ctx) {
      dea_res <- ctx_get_data(ctx, "dea_res")
      if (inherits(dea_res, "glystats_limma_res")) {
        .run_step_volcano_limma(ctx, step_dots)
      } else {
        .run_step_volcano_ttest_wilcox(ctx, step_dots)
      }
    },
    require = "dea_res",
    signature = signature
  )
}

.run_step_volcano_limma <- function(ctx, step_dots) {
  dea_res <- ctx_get_data(ctx, "dea_res")
  contrasts <- .get_unique_contrasts(dea_res)
  for (cont in contrasts) {
    plot_name <- paste0("volcano_", cont)
    p <- .run_function(
      glyvis::plot_volcano,
      dea_res,
      step_id = "volcano",
      global_dots = ctx$dots,
      step_dots = step_dots,
      holy_args = list(contrast = cont)
    )
    ctx <- ctx_add_plot(ctx, plot_name, p, paste0("Volcano plot for the comparison of ", cont, "."))
  }
  ctx
}

.run_step_volcano_ttest_wilcox <- function(ctx, step_dots) {
  dea_res <- ctx_get_data(ctx, "dea_res")
  p <- .run_function(
    glyvis::plot_volcano,
    dea_res,
    step_id = "volcano",
    global_dots = ctx$dots,
    step_dots = step_dots
  )
  ctx_add_plot(ctx, "volcano", p, "Volcano plot")
}

#' Get unique contrasts from DEA results
#' @param dea_res DEA results from `glystats::gly_limma()`
#' @noRd
.get_unique_contrasts <- function(dea_res) {
  checkmate::assert_class(dea_res, "glystats_limma_res")
  tidy_res <- glystats::get_tidy_result(dea_res)
  unique(paste0(tidy_res$ref_group, "_vs_", tidy_res$test_group))
}

#' Step: GO Enrichment Analysis on Differentially Expressed Variables
#'
#' Perform GO enrichment analysis on differentially expressed variables using `glystats::gly_enrich_go()`.
#' This step requires one of the DEA steps to be run.
#' Only execute for glycoproteomics experiments.
#' Use all genes in OrgDb as the background.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to perform GO enrichment analysis for
#' - `sig_exp`: The filtered experiment from DEA, generated by `step_dea_xxx()`.
#'
#' Tables generated:
#' - `go_enrich`: A table containing the GO enrichment results.
#'
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_sig_enrich_go(glystats.gly_enrich_go.p_adj_method = "BH")`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_sig_enrich_go()
#' @seealso [glystats::gly_enrich_go()]
#' @export
step_sig_enrich_go <- function(universe = "all", ...) {
  signature <- rlang::expr_deparse(match.call())
  step_sig_enrich("go", universe = universe, signature = signature, ...)
}

#' Step: KEGG Enrichment Analysis on Differentially Expressed Variables
#'
#' Perform KEGG enrichment analysis on differentially expressed variables using `glystats::gly_enrich_kegg()`.
#' This step requires one of the DEA steps to be run.
#' Only execute for glycoproteomics experiments.
#' Use all genes in OrgDb as the background.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to perform KEGG enrichment analysis for
#' - `sig_exp`: The filtered experiment from DEA, generated by `step_dea_xxx()`.
#'
#' Tables generated:
#' - `kegg_enrich`: A table containing the KEGG enrichment results.
#'
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_sig_enrich_kegg(glystats.gly_enrich_kegg.p_adj_method = "BH")`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_sig_enrich_kegg()
#' @seealso [glystats::gly_enrich_kegg()]
#' @export
step_sig_enrich_kegg <- function(universe = "all", ...) {
  signature <- rlang::expr_deparse(match.call())
  step_sig_enrich("kegg", universe = universe, retry = 2L, signature = signature, ...)
}

#' Step: Reactome Enrichment Analysis on Differentially Expressed Variables
#'
#' Perform Reactome enrichment analysis on differentially expressed variables using `glystats::gly_enrich_reactome()`.
#' This step requires one of the DEA steps to be run.
#' Only execute for glycoproteomics experiments.
#' Use all genes in OrgDb as the background.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to perform Reactome enrichment analysis for
#' - `sig_exp`: The filtered experiment from DEA, generated by `step_dea_xxx()`.
#'
#' Tables generated:
#' - `reactome_enrich`: A table containing the Reactome enrichment results.
#'
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_sig_enrich_reactome(glystats.gly_enrich_reactome.OrgDb = "org.Mm.eg.db")`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_sig_enrich_reactome()
#' @seealso [glystats::gly_enrich_reactome()]
#' @export
step_sig_enrich_reactome <- function(universe = "all", ...) {
  signature <- rlang::expr_deparse(match.call())
  step_sig_enrich("reactome", universe = universe, retry = 2L, signature = signature, ...)
}

#' Step: Enrichment Analysis on Differentially Expressed Variables
#'
#' @description
#' Run functional enrichment analysis on differentially expressed variables using one of:
#' - `glystats::gly_sig_enrich_go()`
#' - `glystats::gly_sig_enrich_kegg()`
#' - `glystats::gly_sig_enrich_reactome()`
#'
#' This step requires one of the DEA steps to be run.
#' Only execute for glycoproteomics experiments.
#' Use all genes in OrgDb as the background.
#'
#' @param kind Enrichment type: `"go"`, `"kegg"`, or `"reactome"`.
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param retry Number of retries if the step errors.
#' @noRd
step_sig_enrich <- function(kind = c("go", "kegg", "reactome"), universe = c("all", "detected"), retry = 0L, signature = NULL, ...) {
  kind <- rlang::arg_match(kind)
  universe <- rlang::arg_match(universe)
  label <- paste0(toupper(kind), " enrichment analysis")
  step_id <- paste0("sig_enrich_", kind)
  step_dots <- rlang::list2(...)
  func <- switch(kind,
    go = "gly_enrich_go",
    kegg = "gly_enrich_kegg",
    reactome = "gly_enrich_reactome"
  )

  step(
    id = step_id,
    label = label,
    condition = function(ctx) {
      check <- glyexp::get_exp_type(ctx_get_data(ctx, "exp")) == "glycoproteomics"
      reason <- "input is not a glycoproteomics experiment"
      list(check = check, reason = reason)
    },
    run = function(ctx) {
      sig_exp <- ctx_get_data(ctx, "sig_exp")
      if (universe == "detected") {
        # Force universe to be the detected experiment, overriding any dots.
        uni_arg <- ctx_get_data(ctx, "exp")
        enrich_res <- switch(
          kind,
          go = .run_function(glystats::gly_enrich_go, sig_exp, step_id, global_dots = ctx$dots, step_dots = step_dots, holy_args = list(universe = uni_arg)),
          kegg = .run_function(glystats::gly_enrich_kegg, sig_exp, step_id, global_dots = ctx$dots, step_dots = step_dots, holy_args = list(universe = uni_arg)),
          reactome = .run_function(glystats::gly_enrich_reactome, sig_exp, step_id, global_dots = ctx$dots, step_dots = step_dots, holy_args = list(universe = uni_arg))
        )
      } else {
        enrich_res <- switch(
          kind,
          go = .run_function(glystats::gly_enrich_go, sig_exp, step_id, global_dots = ctx$dots, step_dots = step_dots),
          kegg = .run_function(glystats::gly_enrich_kegg, sig_exp, step_id, global_dots = ctx$dots, step_dots = step_dots),
          reactome = .run_function(glystats::gly_enrich_reactome, sig_exp, step_id, global_dots = ctx$dots, step_dots = step_dots)
        )
      }
      ctx <- ctx_add_table(
        ctx,
        kind,
        glystats::get_tidy_result(enrich_res),
        paste0(toupper(kind), " enrichment analysis results.")
      )
      p <- .run_function(glyvis::plot_enrich, enrich_res, step_id, global_dots = ctx$dots, step_dots = step_dots)
      ctx_add_plot(ctx, kind, p, paste0(toupper(kind), " enrichment analysis plot."))
    },
    report = function(x) {
      tbl <- x$tables[[kind]]
      n_sig <- sum(tbl$p_adj < 0.05)
      msg <- paste0("Enrichment analysis was performed on differentially expressed variables.")
      if (n_sig > 0) {
        msg <- paste0(msg, " Number of significant items (adjusted p < 0.05): ", n_sig, ".\n\n")
        all_sig_terms <- tbl$description[tbl$p_adj < 0.05]
        all_sig_terms_part <- paste(all_sig_terms, collapse = ", ")
        msg <- paste0(msg, glue::glue("<AI>Summarize these terms in 1 sentence: {all_sig_terms_part}</AI>"))
        msg <- paste0(msg, "Top terms: \n\n", paste("- ", tbl$description[1:min(5, n_sig)], collapse = "\n"), "\n")
      } else {
        msg <- paste0(msg, " No significant items (adjusted p < 0.05).\n")
      }
      msg
    },
    require = c("exp", "sig_exp"),
    retry = retry,
    signature = signature
  )
}

#' Step: Derived Trait Calculation
#'
#' Calculate glycan derived traits using `glydet::derive_traits()`.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to calculate derived traits for
#'
#' Data generated:
#' - `trait_exp`: The experiment with derived traits
#'
#' Tables generated:
#' - `derived_traits`: A table containing the derived traits.
#'
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_derive_traits()
#' @seealso [glydet::derive_traits()]
#' @export
step_derive_traits <- function(...) {
  signature <- rlang::expr_deparse(match.call())
  step_dots <- rlang::list2(...)
  step(
    id = "derive_traits",
    label = "Derived trait calculation",
    condition = function(ctx) {
      check <- "glycan_structure" %in% colnames(ctx_get_data(ctx, "exp")$var_info)
      reason <- "glycan structures are not available in the experiment"
      list(check = check, reason = reason)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      trait_exp <- .run_function(
        glydet::derive_traits,
        exp,
        step_id = "derive_traits",
        global_dots = ctx$dots,
        step_dots = step_dots
      )
      ctx <- ctx_add_data(ctx, "trait_exp", trait_exp)
      ctx <- ctx_add_table(ctx, "derived_traits", tibble::as_tibble(trait_exp), "Derived trait calculation results.")
      ctx
    },
    report = function(x) {
      tbl <- x$tables[["derived_traits"]]
      if (glyexp::get_exp_type(x$exp) == "glycomics") {
        item_name <- "Derived traits"
      } else {
        item_name <- "Site-specific derived traits"
      }
      paste0(
        "Derived traits were calculated and the results were saved in `tables$derived_traits`. ",
        "Number of derived traits: ", length(unique(tbl$trait)), "."
      )
    },
    generate = "trait_exp",
    require = "exp",
    signature = signature
  )
}

#' Step: Heatmap
#'
#' Create a heatmap plot using `glyvis::plot_heatmap()`.
#' The heatmap visualizes expression values across samples.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Plots generated:
#' - `heatmap`: A heatmap plot (if `on = "exp"`)
#' - `sig_heatmap`: A heatmap plot (if `on = "sig_exp"`)
#' - `trait_heatmap`: A heatmap plot (if `on = "trait_exp"`)
#' - `sig_trait_heatmap`: A heatmap plot (if `on = "sig_trait_exp"`)
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "exp", "sig_exp", "trait_exp", or "sig_trait_exp".
#'   Default is "exp".
#' @param ... Step-specific arguments passed to `glyvis::plot_heatmap()`.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_heatmap(glyvis.plot_heatmap.show_rownames = TRUE)`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_heatmap()
#' step_heatmap(on = "sig_exp")
#' step_heatmap(on = "trait_exp")
#' @seealso [glyvis::plot_heatmap()]
#' @export
step_heatmap <- function(on = "exp", ...) {
  on <- rlang::arg_match(on, c("exp", "sig_exp", "trait_exp", "sig_trait_exp"))
  signature <- rlang::expr_deparse(match.call())
  step_dots <- rlang::list2(...)

  # Determine plot name based on `on` parameter
  # "exp" -> "heatmap", "sig_exp" -> "sig_heatmap", etc.
  plot_name <- switch(
    on,
    exp = "heatmap",
    sig_exp = "sig_heatmap",
    trait_exp = "trait_heatmap",
    sig_trait_exp = "sig_trait_heatmap"
  )

  # Determine a readable label for the step
  label <- switch(
    on,
    exp = "Heatmap",
    sig_exp = "Heatmap of significant variables",
    trait_exp = "Heatmap of traits",
    sig_trait_exp = "Heatmap of significant traits"
  )

  step(
    id = paste0("heatmap_", on),
    label = label,
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      p <- .run_function(
        glyvis::plot_heatmap,
        exp,
        step_id = "heatmap",
        global_dots = ctx$dots,
        step_dots = step_dots
      )
      ctx_add_plot(ctx, plot_name, p, paste0("Heatmap of ", on, "."))
    },
    require = on,
    signature = signature
  )
}
