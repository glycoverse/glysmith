#' Differential analysis using limma
#'
#' Fit [glystats::gly_limma()] to the incoming experiment. Connect
#' [step_filter_sig_vars()] to select variables or attach a compatible plot.
#' @inheritParams glystats::gly_limma
#' @param filter_p_adj_cutoff,filter_p_val_cutoff,filter_fc_cutoff Default
#'   adjusted p-value, raw p-value and fold-change cutoffs for downstream filters.
#' @param ... Additional arguments passed to [glystats::gly_limma()].
#' @returns A step producing a fitted differential result.
#' @examples
#' blueprint(step_dea_limma() %=>% step_filter_sig_vars())
#' @export
step_dea_limma <- function(
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
  .check_step_dots(sys.call())
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
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
}

#' Differential analysis using a t-test
#'
#' Fit [glystats::gly_ttest()] to the incoming experiment. Connect
#' [step_filter_sig_vars()] to select variables or attach a compatible plot.
#' @inheritParams glystats::gly_ttest
#' @param filter_p_adj_cutoff,filter_p_val_cutoff,filter_fc_cutoff Default
#'   adjusted p-value, raw p-value and fold-change cutoffs for downstream filters.
#' @param ... Additional arguments passed to [glystats::gly_ttest()].
#' @returns A step producing a fitted differential result.
#' @examples
#' blueprint(step_dea_ttest() %=>% step_filter_sig_vars())
#' @export
step_dea_ttest <- function(
  p_adj_method = "BH",
  ref_group = NULL,
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  ...
) {
  .check_step_dots(sys.call())
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
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
}

#' Differential analysis using ANOVA
#'
#' Fit [glystats::gly_anova()] to the incoming experiment. Connect
#' [step_filter_sig_vars()] to select variables or attach a compatible plot.
#' @inheritParams glystats::gly_anova
#' @param filter_p_adj_cutoff,filter_p_val_cutoff,filter_fc_cutoff Default
#'   adjusted p-value, raw p-value and fold-change cutoffs for downstream filters.
#' @param ... Additional arguments passed to [glystats::gly_anova()].
#' @param filter_on Default test to select in a downstream filter, either
#'   `"main_test"` or `"post_hoc_test"`.
#' @param filter_comparison Default comparison for the post-hoc filter.
#' @returns A step producing a fitted differential result.
#' @examples
#' blueprint(step_dea_anova() %=>% step_filter_sig_vars())
#' @export
step_dea_anova <- function(
  p_adj_method = "BH",
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  filter_on = "main_test",
  filter_comparison = NULL,
  ...
) {
  .check_step_dots(sys.call())
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
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
}

#' Differential analysis using the Wilcoxon test
#'
#' Fit [glystats::gly_wilcox()] to the incoming experiment. Connect
#' [step_filter_sig_vars()] to select variables or attach a compatible plot.
#' @inheritParams glystats::gly_wilcox
#' @param filter_p_adj_cutoff,filter_p_val_cutoff,filter_fc_cutoff Default
#'   adjusted p-value, raw p-value and fold-change cutoffs for downstream filters.
#' @param ... Additional arguments passed to [glystats::gly_wilcox()].
#' @returns A step producing a fitted differential result.
#' @examples
#' blueprint(step_dea_wilcox() %=>% step_filter_sig_vars())
#' @export
step_dea_wilcox <- function(
  p_adj_method = "BH",
  ref_group = NULL,
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  ...
) {
  .check_step_dots(sys.call())
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
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
}

#' Differential analysis using the Kruskal-Wallis test
#'
#' Fit [glystats::gly_kruskal()] to the incoming experiment. Connect
#' [step_filter_sig_vars()] to select variables or attach a compatible plot.
#' @inheritParams glystats::gly_kruskal
#' @param filter_p_adj_cutoff,filter_p_val_cutoff,filter_fc_cutoff Default
#'   adjusted p-value, raw p-value and fold-change cutoffs for downstream filters.
#' @param ... Additional arguments passed to [glystats::gly_kruskal()].
#' @param filter_on Default test to select in a downstream filter, either
#'   `"main_test"` or `"post_hoc_test"`.
#' @param filter_comparison Default comparison for the post-hoc filter.
#' @returns A step producing a fitted differential result.
#' @examples
#' blueprint(step_dea_kruskal() %=>% step_filter_sig_vars())
#' @export
step_dea_kruskal <- function(
  p_adj_method = "BH",
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  filter_on = "main_test",
  filter_comparison = NULL,
  ...
) {
  .check_step_dots(sys.call())
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
    signature = signature,
    dea_args = dea_args,
    filter_args = filter_args
  )
}

.dea_count_sig_total <- function(tbl, id_col = "variable") {
  sig_vars <- tbl[[id_col]][!is.na(tbl$p_adj) & tbl$p_adj < 0.05]
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
#' @param label Analysis label.
#' @returns A character string for the report
#' @noRd
.dea_report <- function(x, method, label) {
  # Get the appropriate table
  if (method %in% c("anova", "kruskal")) {
    tbl <- x$tables$dea_main_test
  } else {
    tbl <- x$tables$dea
  }

  item_name <- "variables"
  id_col <- .stats_id_column(x$data$dea_res)
  sig_total <- .dea_count_sig_total(tbl, id_col)
  is_multigroup <- .dea_is_multigroup(tbl)

  lines <- c(paste0(label, " was performed."))

  if (sig_total == 0) {
    lines <- c(
      lines,
      paste0("No significant ", item_name, " (adjusted p < 0.05).")
    )
    return(paste(lines, collapse = "\n"))
  }

  lines <- c(
    lines,
    paste0(
      "Number of significant ",
      item_name,
      " (adjusted p < 0.05): ",
      sig_total,
      "."
    )
  )

  if (is_multigroup) {
    sig_per_contrast <- .dea_count_sig_per_contrast(tbl)
    if (length(sig_per_contrast) > 0) {
      contrast_lines <- paste0(
        "- ",
        names(sig_per_contrast),
        ": ",
        as.integer(sig_per_contrast)
      )
      lines <- c(lines, "", "Breakdown by contrast:", "", contrast_lines)
    }
  }

  paste(lines, collapse = "\n")
}

.step_dea <- function(method, label, signature, dea_args, filter_args) {
  port <- .exp_port("dea_res")
  port$produces <- if (method %in% c("anova", "kruskal")) {
    "omnibus"
  } else {
    "contrast"
  }
  port$filter_args <- filter_args
  step(
    id = paste0("dea_", method),
    label = label,
    connected = port,
    run = function(ctx) .fit_dea(ctx, ctx$data$exp, method, dea_args),
    report = function(x) .dea_report(x, method, label),
    packages = "glystats",
    signature = signature
  )
}

.fit_dea <- function(ctx, exp, method, dea_args) {
  dea_res <- switch(
    method,
    "limma" = rlang::exec(glystats::gly_limma, exp, !!!dea_args),
    "ttest" = rlang::exec(glystats::gly_ttest, exp, !!!dea_args),
    "anova" = rlang::exec(glystats::gly_anova, exp, !!!dea_args),
    "wilcox" = rlang::exec(glystats::gly_wilcox, exp, !!!dea_args),
    "kruskal" = rlang::exec(glystats::gly_kruskal, exp, !!!dea_args)
  )
  ctx <- ctx_add_data(ctx, "dea_res", dea_res)

  if (method %in% c("anova", "kruskal")) {
    ctx <- ctx_add_table(
      ctx,
      "dea_main_test",
      glystats::get_tidy_result(dea_res, "main_test"),
      paste0(
        "Main test results of ANOVA or Kruskal-Wallis test for ",
        method,
        " analysis."
      )
    )
    ctx <- ctx_add_table(
      ctx,
      "dea_post_hoc_test",
      glystats::get_tidy_result(dea_res, "post_hoc_test"),
      paste0(
        "Post-hoc test results of ANOVA or Kruskal-Wallis test for ",
        method,
        " analysis."
      )
    )
  } else {
    ctx <- ctx_add_table(
      ctx,
      "dea",
      glystats::get_tidy_result(dea_res),
      paste0(
        method,
        " analysis results of all comparisons for all variables."
      )
    )
  }
  ctx
}
