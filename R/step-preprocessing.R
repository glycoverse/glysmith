#' Step: Preprocessing
#'
#' Preprocess the experiment using `glyclean::auto_clean()`.
#' Optionally run QC plots before and/or after preprocessing.
#' This step can be omitted if the experiment is already preprocessed.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to preprocess
#'
#' Data generated:
#' - `raw_exp`: The raw experiment (previous `exp`, saved for reference)
#'
#' Plots generated when `post_qc = TRUE`:
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
#' When `pre_qc = TRUE`, the same plots are generated with the `qc_pre_` prefix.
#'
#' This step is special in that it silently overwrites the `exp` data with the preprocessed experiment.
#' This ensures that no matter if preprocessing is performed or not,
#' the "active" experiment is always under the key `exp`.
#' The previous `exp` is saved as `raw_exp` for reference.
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Always include this step by default unless the user explicitly excludes it
#' or tell you she/he has already performed preprocessing.
#' - Ask the user the column name for batch information if not provided.
#' - Ask the user if there are QC samples in the experiment if not provided. If so, ask the group name of the QC samples.
#'   Explain to the user that if it is "QC" for example, the samples with "QC" in the `group_col` column will be considered as QC samples.
#'   And these QC samples will be used for choosing the best normalization and imputation methods.
#'   Also mention that QC samples will be excluded after preprocessing.
#' - If the user intents to perform biomarker related analysis, set `remove_preset` to "biomarker".
#' - Use default values for other arguments unless the user explicitly specifies otherwise.
#'
#' @param pre_qc Whether to run QC plots before preprocessing.
#' @param post_qc Whether to run QC plots after preprocessing.
#' @param rep_col Column name for replicate information (for `glyclean::plot_rep_scatter()`).
#' @inheritParams glyclean::auto_clean
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_preprocess()
#' step_preprocess(remove_preset = "discovery")
#' @seealso [glyclean::auto_clean()]
#' @export
step_preprocess <- function(
  pre_qc = FALSE,
  post_qc = TRUE,
  batch_col = "batch",
  qc_name = "QC",
  normalize_to_try = NULL,
  impute_to_try = NULL,
  remove_preset = "discovery",
  batch_prop_threshold = 0.3,
  check_batch_confounding = TRUE,
  batch_confounding_threshold = 0.4,
  rep_col = NULL
) {
  signature <- rlang::expr_deparse(match.call())
  run_qc_plots <- function(ctx, exp, stage) {
    if (stage == "pre") {
      plots <- list(
        list(
          id = "qc_missing_heatmap",
          fun = function() glyclean::plot_missing_heatmap(exp),
          desc = "Missing value heatmap."
        ),
        list(
          id = "qc_missing_samples_bar",
          fun = function() glyclean::plot_missing_bar(exp, on = "sample"),
          desc = "Missing value bar plot on samples."
        ),
        list(
          id = "qc_missing_variables_bar",
          fun = function() glyclean::plot_missing_bar(exp, on = "variable"),
          desc = "Missing value bar plot on variables."
        )
      )
    } else {
      plots <- list()
    }
    plots <- c(plots, list(
      list(
        id = "qc_tic_bar",
        fun = function() glyclean::plot_tic_bar(exp),
        desc = "Total intensity count (TIC) bar plot."
      ),
      list(
        id = "qc_rank_abundance",
        fun = function() glyclean::plot_rank_abundance(exp),
        desc = "Rank abundance plot."
      ),
      list(
        id = "qc_int_boxplot",
        fun = function() glyclean::plot_int_boxplot(exp, by = "group"),
        desc = "Log2 intensity boxplot."
      ),
      list(
        id = "qc_rle",
        fun = function() glyclean::plot_rle(exp, by = "group"),
        desc = "Relative Log Expression (RLE) plot."
      ),
      list(
        id = "qc_cv_dent",
        fun = function() glyclean::plot_cv_dent(exp, by = "group"),
        desc = "Coefficient of Variation (CV) density plot."
      ))
    )

    if (!is.null(batch_col) && batch_col %in% colnames(exp$sample_info)) {
      plots <- c(plots, list(list(
        id = "qc_batch_pca",
        fun = function() glyclean::plot_batch_pca(exp, batch_col = batch_col),
        desc = "PCA score plot colored by batch."
      )))
    }

    if (!is.null(rep_col) && rep_col %in% colnames(exp$sample_info)) {
      plots <- c(plots, list(list(
        id = "qc_rep_scatter",
        fun = function() glyclean::plot_rep_scatter(exp, rep_col = rep_col),
        desc = "Replicate scatter plots."
      )))
    }

    id_transform <- function(id) {
      if (stage == "pre") {
        return(sub("^qc_", "qc_pre_", id))
      }
      id
    }

    for (p in plots) {
      plot_id <- id_transform(p$id)
      ctx <- tryCatch({
        plot <- p$fun()
        desc <- p$desc
        if (stage == "pre") {
          desc <- paste0("Preprocess QC (pre): ", desc)
        }
        ctx_add_plot(ctx, plot_id, plot, desc)
      }, error = function(e) {
        cli::cli_warn("Failed to generate plot {plot_id}: {e$message}")
        ctx
      })
    }
    ctx
  }
  step(
    id = "preprocess",
    label = "Preprocessing",
    run = function(ctx) {
      # Here we use a bit of a hack to overwrite the exp data with the preprocessed exp.
      # This ensures the "active" experiment is always under the key "exp",
      # no matter if preprocessing is performed or not.
      exp <- ctx_get_data(ctx, "exp")
      if (isTRUE(pre_qc)) {
        ctx <- run_qc_plots(ctx, exp, "pre")
      }
      clean_exp <- glyclean::auto_clean(
        exp,
        batch_col = batch_col,
        qc_name = qc_name,
        normalize_to_try = normalize_to_try,
        impute_to_try = impute_to_try,
        remove_preset = remove_preset,
        batch_prop_threshold = batch_prop_threshold,
        check_batch_confounding = check_batch_confounding,
        batch_confounding_threshold = batch_confounding_threshold
      )
      ctx <- ctx_add_data(ctx, "exp", clean_exp)  # overwrite exp with preprocessed exp
      ctx <- ctx_add_data(ctx, "raw_exp", exp)  # keep raw exp for reference
      if (isTRUE(post_qc)) {
        ctx <- run_qc_plots(ctx, clean_exp, "post")
      }
      ctx
    },
    report = function(x) {
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
    },
    require = "exp",
    generate = "raw_exp",
    signature = signature
  )
}

#' Step: Subset Groups
#'
#' Subset the experiment to specific groups using the `group` column in sample information.
#' This is useful when downstream steps require exactly two groups for comparison.
#' Usually run after `step_preprocess()` and before DEA or enrichment steps.
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
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Use this step when the experiment has more than 2 groups but the user wants a specific two-group comparison.
#' - Ask the user which two groups to compare, and place this step before DEA and enrichment steps.
#' - Use the order of the user-provided groups to set factor levels.
#'
#' @param groups Group names to keep. If `NULL`, this step will be skipped.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_subset_groups(groups = c("H", "C"))
#' @export
step_subset_groups <- function(groups = NULL) {
  checkmate::assert_character(groups, min.len = 1, unique = TRUE, null.ok = TRUE)
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "subset_groups",
    label = "Subset groups",
    condition = function(ctx) {
      if (is.null(groups)) {
        return(list(check = FALSE, reason = "group subset is not provided"))
      }
      list(check = TRUE, reason = NULL)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      available <- unique(as.character(exp$sample_info$group))
      missing <- setdiff(groups, available)
      if (length(missing) > 0) {
        missing_txt <- paste(missing, collapse = ", ")
        cli::cli_abort("Group(s) not found in experiment: {missing_txt}.")
      }

      full_exp <- exp
      exp <- exp |>
        glyexp::filter_obs(.data$group %in% groups)
      exp$sample_info$group <- factor(exp$sample_info$group, levels = groups)

      ctx <- ctx_add_data(ctx, "exp", exp)
      ctx <- ctx_add_data(ctx, "full_exp", full_exp)
      ctx
    },
    require = "exp",
    generate = "full_exp",
    signature = signature
  )
}

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
      cli::cli_abort("Protein expression file must have at least two columns (protein + samples).")
    }
    protein <- tbl[[1]]
    if (any(is.na(protein) | !nzchar(protein))) {
      cli::cli_abort("Protein accessions in the first column must be non-empty.")
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
      if (is.null(rownames(obj)) || any(is.na(rownames(obj)) | !nzchar(rownames(obj)))) {
        cli::cli_abort("RDS data.frame must have non-empty row names (protein accessions).")
      }
      obj <- as.matrix(obj)
    }
    if (!is.matrix(obj)) {
      cli::cli_abort("RDS file must contain a matrix or data.frame with row names.")
    }
    if (is.null(rownames(obj)) || any(is.na(rownames(obj)) | !nzchar(rownames(obj)))) {
      cli::cli_abort("Protein accessions in row names must be non-empty.")
    }
    if (anyDuplicated(rownames(obj)) > 0) {
      cli::cli_abort("Protein accessions in row names must be unique.")
    }
    return(obj)
  }

  cli::cli_abort("Unsupported protein expression file extension: {.val {ext}}. Use .csv, .tsv, or .rds.")
}

#' Step: Adjust Protein Abundance
#'
#' Adjust glycoform quantification values by correcting for protein abundance
#' utilizing `glyclean::adjust_protein()`.
#' Usually this step should be run after `step_preprocess()`.
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
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only if the user explicitly asks for protein adjustment.
#' - If protein adjustment is needed and the `pro_expr_path` is not provided, ask for it and explain how to prepare the file:
#'   - CSV/TSV: first column is protein accessions; remaining columns are sample names.
#'   - RDS: a matrix/data.frame with row names as protein accessions and columns as sample names.
#' - You MUST provide a detailed explanation of how to prepare the file.
#' - With out the file, the step is invalid.
#'
#' @param pro_expr_path Path to the protein expression matrix file.
#'   If `NULL`, this step will be skipped.
#'   Can be:
#'   - A CSV/TSV file with the first column as protein accessions and remaining columns as sample names.
#'   - An RDS file with a matrix or data.frame with row names as protein accessions and columns as sample names.
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
step_adjust_protein <- function(pro_expr_path = NULL, method = "ratio") {
  if (!is.null(pro_expr_path)) {
    checkmate::assert_file_exists(pro_expr_path, access = "r", extension = c("csv", "tsv", "rds"))
  }
  checkmate::assert_choice(method, c("ratio", "reg"))
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "adjust_protein",
    label = "Protein adjustment",
    condition = function(ctx) {
      if (is.null(pro_expr_path)) {
        return(list(check = FALSE, reason = "protein expression path is not provided"))
      }
      if (glyexp::get_exp_type(ctx_get_data(ctx, "exp")) != "glycoproteomics") {
        return(list(check = FALSE, reason = "input is not a glycoproteomics experiment"))
      }
      list(check = TRUE, reason = NULL)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      pro_expr_mat <- .read_pro_expr_mat(pro_expr_path)
      adj_exp <- glyclean::adjust_protein(exp, pro_expr_mat, method = method)
      ctx <- ctx_add_data(ctx, "exp", adj_exp)
      ctx <- ctx_add_data(ctx, "unadj_exp", exp)
      ctx
    },
    report = function(x) {
      logs <- x$meta$logs$adjust_protein %||% list()
      msg_lines <- logs$message %||% character(0)
      if (length(msg_lines) == 0) {
        msg_lines <- logs$output %||% character(0)
      }
      base <- "Protein-adjusted glycoform quantification was performed."
      if (length(msg_lines) == 0) return(base)
      paste(c(base, msg_lines), collapse = "\n")
    },
    require = "exp",
    generate = "unadj_exp",
    signature = signature
  )
}

#' Step: Identification Overview
#'
#' Summarize the experiment using `glyexp::summarize_experiment()`.
#' This is usually the first step, BEFORE `step_preprocess()`.
#' Very light-weight to run, so always include it.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to summarize
#'
#' Tables generated:
#' - `summary`: A table containing the identification overview of the experiment
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Always include this step by default unless the user explicitly excludes it.
#' - Use it as the first step in the blueprint.
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
    label = "Identification overview",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      tbl <- glyexp::summarize_experiment(exp, count_struct = count_struct)
      ctx_add_table(ctx, "summary", tbl, "Identification overview of the experiment.")
    },
    report = function(x) {
      tbl <- x$tables[["summary"]]
      total_tbl <- dplyr::filter(tbl, stringr::str_starts(.data$item, "total_"))
      total_parts <- paste0(total_tbl$n, " ", stringr::str_remove(total_tbl$item, "total_"), "s")
      sample_tbl <- dplyr::filter(tbl, stringr::str_ends(.data$item, "_per_sample"))
      sample_parts <- paste0(sample_tbl$n, " ", stringr::str_remove(sample_tbl$item, "_per_sample"), "s")
      glue::glue("In total, there are {paste(total_parts, collapse = ', ')}. On average, there are {paste(sample_parts, collapse = ', ')} per sample.")
    },
    require = "exp",
    signature = signature
  )
}
