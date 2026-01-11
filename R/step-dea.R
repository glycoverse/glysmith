#' Step: Differential Expression Analysis (DEA) using Limma
#'
#' Run differential analysis using linear model-based analysis via `glystats::gly_limma()`,
#' then filter the experiment to keep only the differentially expressed variables using `glystats::filter_sig_vars()`.
#' By default, this runs DEA on the main experiment (`exp`), but can be configured
#' to run on derived traits (`trait_exp`) or other experiment objects.
#' This step is the recommended DEA method for all experiments,
#' for both two-group and multi-group experiments.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to run DEA on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run DEA on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to run DEA on
#'
#' Data generated:
#' - `dea_res`: The DEA (differential expression analysis) results (if `on = "exp"`, default)
#' - `dta_res`: The DTA (differential trait analysis) results (if `on = "trait_exp"`)
#' - `dma_res`: The DMA (differential motif analysis) results (if `on = "motif_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#' - `sig_motif_exp`: The filtered motif experiment (if `on = "motif_exp"`)
#'
#' Tables generated:
#' - `dea`: A table containing the DEA (differential expression analysis) result (if `on = "exp"`, default)
#' - `dta`: A table containing the DTA (differential trait analysis) result (if `on = "trait_exp"`)
#' - `dma`: A table containing the DMA (differential motif analysis) result (if `on = "motif_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Use this step to perform DEA by default, unless the user asks for other methods.
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#'   Use `"motif_exp"` for differential motif analysis.
#' @inheritParams glystats::gly_limma
#' @param filter_p_adj_cutoff Adjusted p-value cutoff for filtering.
#' @param filter_p_val_cutoff Raw p-value cutoff for filtering.
#' @param filter_fc_cutoff Fold change cutoff for filtering.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_limma()
#' step_dea_limma(on = "trait_exp")  # Differential trait analysis
#' step_dea_limma(p_adj_method = "BH")
#' @seealso [glystats::gly_limma()]
#' @export
step_dea_limma <- function(
  on = "exp",
  p_adj_method = "BH",
  covariate_cols = NULL,
  subject_col = NULL,
  ref_group = NULL,
  contrasts = NULL,
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  ...
) {
  signature <- rlang::expr_deparse(match.call())
  dea_args <- rlang::list2(
    p_adj_method = p_adj_method,
    covariate_cols = covariate_cols,
    subject_col = subject_col,
    ref_group = ref_group,
    contrasts = contrasts,
    ...
  )
  filter_args <- list(
    p_adj_cutoff = filter_p_adj_cutoff,
    p_val_cutoff = filter_p_val_cutoff,
    fc_cutoff = filter_fc_cutoff
  )
  .step_dea(
    method = "limma",
    label = "Differential expression analysis (limma)",
    on = on,
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
}

#' Step: Differential Expression Analysis (DEA) using t-test
#'
#' Run differential analysis using t-test via `glystats::gly_ttest()`,
#' then filter the experiment to keep only the differentially expressed variables using `glystats::filter_sig_vars()`.
#' By default, this runs DEA on the main experiment (`exp`), but can be configured
#' to run on derived traits (`trait_exp`) or other experiment objects.
#' Only use this method for experiments with 2 groups.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Data generated:
#' - `dea_res`: The DEA results (if `on = "exp"`, default)
#' - `dta_res`: The DTA results (if `on = "trait_exp"`)
#' - `dma_res`: The DMA results (if `on = "motif_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#' - `sig_motif_exp`: The filtered motif experiment (if `on = "motif_exp"`)
#'
#' Tables generated:
#' - `dea`: A table containing the DEA result (if `on = "exp"`, default)
#' - `dta`: A table containing the DTA result (if `on = "trait_exp"`)
#' - `dma`: A table containing the DMA result (if `on = "motif_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only if the user explicitly asks for t-test.
#' - If the experiment has more than 2 groups but the user wants a specific two-group
#'   comparison, ask which two groups to compare and include
#'   `step_subset_groups(groups = c("A", "B"))` before this step.
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#'   Use `"motif_exp"` for differential motif analysis.
#' @inheritParams glystats::gly_ttest
#' @param filter_p_adj_cutoff Adjusted p-value cutoff for filtering.
#' @param filter_p_val_cutoff Raw p-value cutoff for filtering.
#' @param filter_fc_cutoff Fold change cutoff for filtering.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_ttest()
#' step_dea_ttest(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_ttest()]
#' @export
step_dea_ttest <- function(
  on = "exp",
  p_adj_method = "BH",
  ref_group = NULL,
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  ...
) {
  signature <- rlang::expr_deparse(match.call())
  dea_args <- rlang::list2(
    p_adj_method = p_adj_method,
    ref_group = ref_group,
    ...
  )
  filter_args <- list(
    p_adj_cutoff = filter_p_adj_cutoff,
    p_val_cutoff = filter_p_val_cutoff,
    fc_cutoff = filter_fc_cutoff
  )
  .step_dea(
    method = "ttest",
    label = "Differential expression analysis (t-test)",
    on = on,
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
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
#' - `dma_res`: The DMA results (if `on = "motif_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#' - `sig_motif_exp`: The filtered motif experiment (if `on = "motif_exp"`)
#'
#' Tables generated:
#' - `dea_main_test`, `dea_post_hoc_test`: Tables containing the results (if `on = "exp"`, default)
#' - `dta_main_test`, `dta_post_hoc_test`: Tables containing the results (if `on = "trait_exp"`)
#' - `dma_main_test`, `dma_post_hoc_test`: Tables containing the results (if `on = "motif_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only if the user explicitly asks for ANOVA.
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#'   Use `"motif_exp"` for differential motif analysis.
#' @inheritParams glystats::gly_anova
#' @param filter_p_adj_cutoff Adjusted p-value cutoff for filtering.
#' @param filter_p_val_cutoff Raw p-value cutoff for filtering.
#' @param filter_fc_cutoff Fold change cutoff for filtering.
#' @param filter_on Name of the test to filter on. Default is `"main_test"`. Can also be `"post_hoc_test"`.
#' @param filter_comparison Name of the comparison to filter on.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_anova()
#' step_dea_anova(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_anova()]
#' @export
step_dea_anova <- function(
  on = "exp",
  p_adj_method = "BH",
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  filter_on = "main_test",
  filter_comparison = NULL,
  ...
) {
  signature <- rlang::expr_deparse(match.call())
  dea_args <- rlang::list2(p_adj_method = p_adj_method, ...)
  filter_args <- list(
    p_adj_cutoff = filter_p_adj_cutoff,
    p_val_cutoff = filter_p_val_cutoff,
    fc_cutoff = filter_fc_cutoff,
    on = filter_on,
    comparison = filter_comparison
  )
  .step_dea(
    method = "anova",
    label = "Differential expression analysis (ANOVA)",
    on = on,
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
}

#' Step: Differential Expression Analysis (DEA) using Wilcoxon test
#'
#' Run differential analysis using Wilcoxon analysis via `glystats::gly_wilcox()`,
#' then filter the experiment to keep only the differentially expressed variables using `glystats::filter_sig_vars()`.
#' By default, this runs DEA on the main experiment (`exp`), but can be configured
#' to run on derived traits (`trait_exp`) or other experiment objects.
#' Only use this method for experiments with 2 groups.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Data generated:
#' - `dea_res`: The DEA results (if `on = "exp"`, default)
#' - `dta_res`: The DTA results (if `on = "trait_exp"`)
#' - `dma_res`: The DMA results (if `on = "motif_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#' - `sig_motif_exp`: The filtered motif experiment (if `on = "motif_exp"`)
#'
#' Tables generated:
#' - `dea`: A table containing the DEA result (if `on = "exp"`, default)
#' - `dta`: A table containing the DTA result (if `on = "trait_exp"`)
#' - `dma`: A table containing the DMA result (if `on = "motif_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only if the user explicitly asks for Wilcoxon test.
#' - If the experiment has more than 2 groups but the user wants a specific two-group
#'   comparison, ask which two groups to compare and include
#'   `step_subset_groups(groups = c("A", "B"))` before this step.
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#'   Use `"motif_exp"` for differential motif analysis.
#' @inheritParams glystats::gly_wilcox
#' @param filter_p_adj_cutoff Adjusted p-value cutoff for filtering.
#' @param filter_p_val_cutoff Raw p-value cutoff for filtering.
#' @param filter_fc_cutoff Fold change cutoff for filtering.
#' @param ... Additional arguments passed to [glystats::gly_wilcox()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_wilcox()
#' step_dea_wilcox(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_wilcox()]
#' @export
step_dea_wilcox <- function(
  on = "exp",
  p_adj_method = "BH",
  ref_group = NULL,
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  ...
) {
  signature <- rlang::expr_deparse(match.call())
  dea_args <- rlang::list2(
    p_adj_method = p_adj_method,
    ref_group = ref_group,
    ...
  )
  filter_args <- list(
    p_adj_cutoff = filter_p_adj_cutoff,
    p_val_cutoff = filter_p_val_cutoff,
    fc_cutoff = filter_fc_cutoff
  )
  .step_dea(
    method = "wilcox",
    label = "Differential expression analysis (Wilcoxon)",
    on = on,
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
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
#' - `dma_res`: The DMA results (if `on = "motif_exp"`)
#' - `sig_exp`: The filtered experiment (if `on = "exp"`, default)
#' - `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)
#' - `sig_motif_exp`: The filtered motif experiment (if `on = "motif_exp"`)
#'
#' Tables generated:
#' - `dea_main_test`, `dea_post_hoc_test`: Tables containing the results (if `on = "exp"`, default)
#' - `dta_main_test`, `dta_post_hoc_test`: Tables containing the results (if `on = "trait_exp"`)
#' - `dma_main_test`, `dma_post_hoc_test`: Tables containing the results (if `on = "motif_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only if the user explicitly asks for Kruskal-Wallis test.
#'
#' @param on Name of the experiment data in `ctx$data` to run analysis on.
#'   Default is `"exp"` for differential expression analysis.
#'   Use `"trait_exp"` for differential trait analysis.
#'   Use `"motif_exp"` for differential motif analysis.
#' @inheritParams glystats::gly_kruskal
#' @param filter_p_adj_cutoff Adjusted p-value cutoff for filtering.
#' @param filter_p_val_cutoff Raw p-value cutoff for filtering.
#' @param filter_fc_cutoff Fold change cutoff for filtering.
#' @param filter_on Filter on "main_test" or "post_hoc_test" for Kruskal-Wallis results.
#' @param filter_comparison Comparison name for post-hoc filtering.
#' @param ... Additional arguments passed to [glystats::gly_kruskal()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea_kruskal()
#' step_dea_kruskal(on = "trait_exp")  # Differential trait analysis
#' @seealso [glystats::gly_kruskal()]
#' @export
step_dea_kruskal <- function(
  on = "exp",
  p_adj_method = "BH",
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  filter_on = "main_test",
  filter_comparison = NULL,
  ...
) {
  signature <- rlang::expr_deparse(match.call())
  dea_args <- rlang::list2(p_adj_method = p_adj_method, ...)
  filter_args <- list(
    p_adj_cutoff = filter_p_adj_cutoff,
    p_val_cutoff = filter_p_val_cutoff,
    fc_cutoff = filter_fc_cutoff,
    on = filter_on,
    comparison = filter_comparison
  )
  .step_dea(
    method = "kruskal",
    label = "Differential expression analysis (Kruskal-Wallis)",
    on = on,
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
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
    ),
    motif_exp = list(
      prefix = "dma",
      label = "Differential motif",
      require = "motif_exp",
      name = "motif"
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
  sig_vars <- tbl$variable[!is.na(tbl$p_adj) & tbl$p_adj < 0.05]
  length(unique(sig_vars))
}

#' Count significant variables per contrast
#' @param tbl A tibble with DEA results (must have ref_group and test_group columns)
#' @returns A named numeric vector with counts per contrast
#' @noRd
.dea_count_sig_per_contrast <- function(tbl) {
  sig_tbl <- tbl[!is.na(tbl$p_adj) & tbl$p_adj < 0.05, , drop = FALSE]
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

  lines <- c(paste0(meta$label, " analysis was performed."))

  if (sig_total == 0) {
    lines <- c(lines, paste0("No significant ", item_name, " (adjusted p < 0.05)."))
    return(paste(lines, collapse = "\n"))
  }

  lines <- c(lines, paste0("Number of significant ", item_name, " (adjusted p < 0.05): ", sig_total, "."))

  if (is_multigroup) {
    sig_per_contrast <- .dea_count_sig_per_contrast(tbl)
    if (length(sig_per_contrast) > 0) {
      contrast_lines <- paste0("- ", names(sig_per_contrast), ": ", as.integer(sig_per_contrast))
      lines <- c(lines, "", "Breakdown by contrast:", "", contrast_lines)
    }
  }

  paste(lines, collapse = "\n")
}

#' Internal helper for DEA steps
#' @noRd
.step_dea <- function(method, label, on = "exp", signature = NULL, dea_args, filter_args) {
  meta <- .get_dea_meta(on)
  dea_args <- dea_args %||% list()
  filter_args <- filter_args %||% list()

  step(
    id = paste0(meta$prefix, "_", method),
    label = paste0(meta$label, " analysis (", method, ")"),
    run = function(ctx) {
      rlang::check_installed("FSA")
      exp <- ctx_get_data(ctx, on)
      # Apply filtering for trait_exp if needed
      if (on == "trait_exp") {
        exp <- glyclean::remove_constant(exp)
      }

      dea_res <- switch(
        method,
        "limma" = rlang::exec(glystats::gly_limma, exp, !!!dea_args),
        "ttest" = rlang::exec(glystats::gly_ttest, exp, !!!dea_args),
        "anova" = rlang::exec(glystats::gly_anova, exp, !!!dea_args),
        "wilcox" = rlang::exec(glystats::gly_wilcox, exp, !!!dea_args),
        "kruskal" = rlang::exec(glystats::gly_kruskal, exp, !!!dea_args)
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
      sig_exp <- rlang::exec(
        glystats::filter_sig_vars,
        exp,
        res = dea_res,
        !!!filter_args
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
