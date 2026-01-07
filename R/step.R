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
    step_adjust_protein(),
    step_ident_overview(),
    step_pca(),
    step_tsne(),
    step_umap(),
    step_heatmap(),
    step_logo(),
    step_dea_limma(),
    step_dea_ttest(),
    step_dea_wilcox(),
    step_dea_anova(),
    step_dea_kruskal(),
    step_volcano(),
    step_sig_enrich_go(),
    step_sig_enrich_kegg(),
    step_sig_enrich_reactome(),
    step_derive_traits(),
    step_quantify_motifs(),
    step_roc()
  )
  names(steps) <- purrr::map_chr(steps, "id")
  steps
}

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
      ),
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
      )
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

#' Step: Principal Component Analysis (PCA)
#'
#' Run PCA using `glystats::gly_pca()` and plot it with `glyvis::plot_pca()`.
#' Loading plot for glycoproteomics data can be crowded with too many variables.
#' Ignore the resulting plot if it is not informative.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to run PCA on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to run PCA on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to run PCA on
#'
#' Tables generated (with suffixes):
#' - `pca_samples`: A table containing the PCA scores for each sample
#' - `pca_variables`: A table containing the PCA loadings for each variable
#' - `pca_eigenvalues`: A table containing the PCA eigenvalues
#'
#' Plots generated (with suffixes):
#' - `pca_scores`: A PCA score plot colored by group
#' - `pca_loadings`: A PCA loading plot
#' - `pca_screeplot`: A PCA screeplot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#'
#' @param on Name of the experiment to run PCA on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
#' @inheritParams glystats::gly_pca
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_pca()
#' @seealso [glystats::gly_pca()], [glyvis::plot_pca()]
#' @export
step_pca <- function(
  on = "exp",
  center = TRUE,
  scale = TRUE,
  ...
) {
  rlang::check_installed("factoextra")
  signature <- rlang::expr_deparse(match.call())
  pca_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("pca", on_meta$id_suffix)
  step(
    id = id,
    label = paste0("Principal component analysis", on_meta$label_suffix),
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      pca_res <- rlang::exec(
        glystats::gly_pca,
        exp,
        center = center,
        scale = scale,
        !!!pca_args
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_samples"),
        glystats::get_tidy_result(pca_res, "samples"),
        paste0("PCA scores for each sample of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_variables"),
        glystats::get_tidy_result(pca_res, "variables"),
        paste0("PCA loadings for each variable of ", on, ".")
      )
      ctx <- ctx_add_table(
        ctx,
        paste0(id, "_eigenvalues"),
        glystats::get_tidy_result(pca_res, "eigenvalues"),
        paste0("PCA eigenvalues of ", on, ".")
      )
      p_scores <- glyvis::plot_pca(pca_res, type = "individual")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_scores"),
        p_scores,
        paste0("PCA score plot colored by group of ", on, ".")
      )
      p_loadings <- glyvis::plot_pca(pca_res, type = "variables")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_loadings"),
        p_loadings,
        paste0("PCA loading plot of ", on, ".")
      )
      p_screeplot <- glyvis::plot_pca(pca_res, type = "screeplot")
      ctx <- ctx_add_plot(
        ctx,
        paste0(id, "_screeplot"),
        p_screeplot,
        paste0("PCA screeplot of ", on, ".")
      )
      ctx
    },
    require = on,
    signature = signature
  )
}

#' Step: t-SNE
#'
#' Perform t-SNE analysis using `glystats::gly_tsne()` and
#' plot a t-SNE plot using `glyvis::plot_tsne()`.
#' Note that the result of t-SNE largely depends on the `perplexity` parameter.
#' Usually it's a trial-and-error process to find the best value iteratively.
#' If you are not satisfied with the result,
#' manually call `glyvis::plot_tsne()` with different `perplexity` values
#' to find the best one.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to perform t-SNE on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform t-SNE on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to perform t-SNE on
#'
#' Data generated (with suffixes):
#' - `tsne`: The t-SNE result
#'
#' Plots generated (with suffixes):
#' - `tsne`: The t-SNE plot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only when the user explicitly asks for t-SNE.
#'
#' @param on Name of the experiment to run t-SNE on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
#' @inheritParams glystats::gly_tsne
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_tsne()
#' step_tsne(perplexity = 30)
#' @seealso [glystats::gly_tsne()], [glyvis::plot_tsne()]
#' @export
step_tsne <- function(
  on = "exp",
  dims = 2,
  perplexity = 30,
  ...
) {
  rlang::check_installed("Rtsne")
  signature <- rlang::expr_deparse(match.call())
  tsne_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("tsne", on_meta$id_suffix)

  step(
    id = id,
    label = "t-SNE",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      tsne <- rlang::exec(
        glystats::gly_tsne,
        exp,
        dims = dims,
        perplexity = perplexity,
        !!!tsne_args
      )
      ctx <- ctx_add_table(ctx, id, glystats::get_tidy_result(tsne), paste0("t-SNE result of ", on, "."))
      p <- glyvis::plot_tsne(tsne)
      ctx <- ctx_add_plot(ctx, id, p, paste0("t-SNE plot of ", on, "."))
      ctx
    },
    require = on,
    signature = signature
  )
}

#' Step: UMAP
#'
#' Perform UMAP analysis using `glystats::gly_umap()` and
#' plot a UMAP plot using `glyvis::plot_umap()`.
#' Note that the result of UMAP largely depends on the `n_neighbors` parameter.
#' Usually it's a trial-and-error process to find the best value iteratively.
#' If you are not satisfied with the result,
#' manually call `glyvis::plot_umap()` with different `n_neighbors` values
#' to find the best one.
#'
#' @details
#' Data required:
#' - `exp` (if `on = "exp"`): The experiment to perform UMAP on
#' - `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform UMAP on
#' - `motif_exp` (if `on = "motif_exp"`): The motif experiment to perform UMAP on
#'
#' Data generated (with suffixes):
#' - `umap`: The UMAP result
#'
#' Plots generated (with suffixes):
#' - `umap`: The UMAP plot
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step only when the user explicitly asks for UMAP.
#'
#' @param on Name of the experiment to run UMAP on.
#'   Can be "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
#' @inheritParams glystats::gly_umap
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_umap()
#' step_umap(n_neighbors = 15)
#' @seealso [glystats::gly_umap()], [glyvis::plot_umap()]
#' @export
step_umap <- function(
  on = "exp",
  n_neighbors = 15,
  n_components = 2,
  ...
) {
  rlang::check_installed("uwot")
  signature <- rlang::expr_deparse(match.call())
  umap_args <- rlang::list2(...)
  on_meta <- .resolve_on(on)
  id <- paste0("umap", on_meta$id_suffix)

  step(
    id = id,
    label = "UMAP",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      umap <- rlang::exec(
        glystats::gly_umap,
        exp,
        n_neighbors = n_neighbors,
        n_components = n_components,
        !!!umap_args
      )
      ctx <- ctx_add_table(ctx, id, glystats::get_tidy_result(umap), paste0("UMAP result of ", on, "."))
      p <- glyvis::plot_umap(umap)
      ctx <- ctx_add_plot(ctx, id, p, paste0("UMAP plot of ", on, "."))
      ctx
    },
    require = on,
    signature = signature
  )
}

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
  rlang::check_installed("FSA")
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

#' Step: Volcano Plot
#'
#' Create a volcano plot from DEA results using `glyvis::plot_volcano()`.
#' This step requires one of the DEA steps to be run:
#' - [step_dea_limma()] (multi-group comparison is also supported)
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
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Always include this step by default if DEA is performed, and the DEA method is not ANOVA or Kruskal-Wallis.
#'
#' @inheritParams glyvis::plot_volcano
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_volcano()
#' step_volcano(log2fc_cutoff = 2)
#' @seealso [glyvis::plot_volcano()]
#' @export
step_volcano <- function(log2fc_cutoff = 1, p_cutoff = 0.05, p_col = "p_adj", ...) {
  rlang::check_installed("EnhancedVolcano")
  signature <- rlang::expr_deparse(match.call())
  step(
    id = "volcano",
    label = "Volcano plot",
    condition = function(ctx) {
      dea_res <- ctx_get_data(ctx, "dea_res")
      check <- !inherits(dea_res, "glystats_anova_res") &&
        !inherits(dea_res, "glystats_kruskal_res")
      reason <- "volcano plot is not supported for ANOVA or Kruskal-Wallis DEA results."
      list(check = check, reason = reason)
    },
    run = function(ctx) {
      dea_res <- ctx_get_data(ctx, "dea_res")
      if (inherits(dea_res, "glystats_limma_res")) {
        .run_step_volcano_limma(ctx, log2fc_cutoff, p_cutoff, p_col, ...)
      } else {
        .run_step_volcano_ttest_wilcox(ctx, log2fc_cutoff, p_cutoff, p_col, ...)
      }
    },
    require = "dea_res",
    signature = signature
  )
}

.run_step_volcano_limma <- function(ctx, log2fc_cutoff, p_cutoff, p_col, ...) {
  dea_res <- ctx_get_data(ctx, "dea_res")
  contrasts <- .get_unique_contrasts(dea_res)
  for (cont in contrasts) {
    plot_name <- paste0("volcano_", cont)
    p <- glyvis::plot_volcano(
      dea_res,
      log2fc_cutoff = log2fc_cutoff,
      p_cutoff = p_cutoff,
      p_col = p_col,
      contrast = cont,
      ...
    )
    ctx <- ctx_add_plot(ctx, plot_name, p, paste0("Volcano plot for the comparison of ", cont, "."))
  }
  ctx
}

.run_step_volcano_ttest_wilcox <- function(ctx, log2fc_cutoff, p_cutoff, p_col, ...) {
  dea_res <- ctx_get_data(ctx, "dea_res")
  p <- glyvis::plot_volcano(
    dea_res,
    log2fc_cutoff = log2fc_cutoff,
    p_cutoff = p_cutoff,
    p_col = p_col,
    ...
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
#' Only execute for glycoproteomics experiments with exactly 2 groups.
#' If used for glycomics experiments, the step will be skipped.
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
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#' - Leave `universe` to "all" (by default) unless the user explicitly mentions that
#'   the background should be the detected variables in `exp`.
#'
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param plot_type Plot type for enrichment results ("dotplot", "barplot", etc.).
#' @param ... Additional arguments passed to [glystats::gly_enrich_go()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_sig_enrich_go()
#' step_sig_enrich_go(plot_type = "barplot")
#' @seealso [glystats::gly_enrich_go()]
#' @export
step_sig_enrich_go <- function(universe = "all", plot_type = "dotplot", ...) {
  rlang::check_installed("clusterProfiler")
  rlang::check_installed("org.Hs.eg.db")
  signature <- rlang::expr_deparse(match.call())
  step_sig_enrich(
    "go",
    universe = universe,
    plot_type = plot_type,
    signature = signature,
    ...
  )
}

#' Step: KEGG Enrichment Analysis on Differentially Expressed Variables
#'
#' Perform KEGG enrichment analysis on differentially expressed variables using `glystats::gly_enrich_kegg()`.
#' This step requires one of the DEA steps to be run.
#' Only execute for glycoproteomics experiments with exactly 2 groups.
#' If used for glycomics experiments, the step will be skipped.
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
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#' - Leave `universe` to "all" (by default) unless the user explicitly mentions that
#'   the background should be the detected variables in `exp`.
#'
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param plot_type Plot type for enrichment results ("dotplot", "barplot", etc.).
#' @param ... Additional arguments passed to [glystats::gly_enrich_kegg()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_sig_enrich_kegg()
#' step_sig_enrich_kegg(plot_type = "barplot")
#' @seealso [glystats::gly_enrich_kegg()]
#' @export
step_sig_enrich_kegg <- function(universe = "all", plot_type = "dotplot", ...) {
  rlang::check_installed("clusterProfiler")
  rlang::check_installed("org.Hs.eg.db")
  signature <- rlang::expr_deparse(match.call())
  step_sig_enrich(
    "kegg",
    universe = universe,
    plot_type = plot_type,
    retry = 2L,
    signature = signature,
    ...
  )
}

#' Step: Reactome Enrichment Analysis on Differentially Expressed Variables
#'
#' Perform Reactome enrichment analysis on differentially expressed variables using `glystats::gly_enrich_reactome()`.
#' This step requires one of the DEA steps to be run.
#' Only execute for glycoproteomics experiments with exactly 2 groups.
#' If used for glycomics experiments, the step will be skipped.
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
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#' - Leave `universe` to "all" (by default) unless the user explicitly mentions that
#'   the background should be the detected variables in `exp`.
#'
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param plot_type Plot type for enrichment results ("dotplot", "barplot", etc.).
#' @param ... Additional arguments passed to [glystats::gly_enrich_reactome()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_sig_enrich_reactome()
#' step_sig_enrich_reactome(plot_type = "barplot")
#' @seealso [glystats::gly_enrich_reactome()]
#' @export
step_sig_enrich_reactome <- function(universe = "all", plot_type = "dotplot", ...) {
  rlang::check_installed("clusterProfiler")
  rlang::check_installed("org.Hs.eg.db")
  signature <- rlang::expr_deparse(match.call())
  step_sig_enrich(
    "reactome",
    universe = universe,
    plot_type = plot_type,
    retry = 2L,
    signature = signature,
    ...
  )
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
#' Only execute for glycoproteomics experiments with exactly 2 groups.
#' Use all genes in OrgDb as the background.
#'
#' @param kind Enrichment type: `"go"`, `"kegg"`, or `"reactome"`.
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param retry Number of retries if the step errors.
#' @noRd
step_sig_enrich <- function(
  kind = c("go", "kegg", "reactome"),
  universe = c("all", "detected"),
  plot_type = "dotplot",
  retry = 0L,
  signature = NULL,
  ...
) {
  kind <- rlang::arg_match(kind)
  universe <- rlang::arg_match(universe)
  label <- paste0(toupper(kind), " enrichment analysis")
  step_id <- paste0("sig_enrich_", kind)
  enrich_args <- rlang::list2(...)

  step(
    id = step_id,
    label = label,
    condition = function(ctx) {
      check1 <- glyexp::get_exp_type(ctx_get_data(ctx, "exp")) == "glycoproteomics"
      reason1 <- "input is not a glycoproteomics experiment"
      check2 <- length(unique(ctx_get_data(ctx, "exp")$sample_info$group)) == 2
      reason2 <- "input has more than 2 groups"
      if (check1 && check2) {
        list(check = TRUE, reason = NULL)
      } else if (check1 && !check2) {
        list(check = FALSE, reason = reason2)
      } else if (!check1 && check2) {
        list(check = FALSE, reason = reason1)
      } else {
        list(check = FALSE, reason = paste0(reason1, " and ", reason2))
      }
    },
    run = function(ctx) {
      sig_exp <- ctx_get_data(ctx, "sig_exp")
      call_args <- enrich_args
      if (universe == "detected") {
        # Force universe to be the detected experiment, overriding any dots.
        uni_arg <- ctx_get_data(ctx, "exp")
        call_args <- c(call_args, list(universe = uni_arg))
      }
      enrich_res <- switch(
        kind,
        go = rlang::exec(glystats::gly_enrich_go, sig_exp, !!!call_args),
        kegg = rlang::exec(glystats::gly_enrich_kegg, sig_exp, !!!call_args),
        reactome = rlang::exec(glystats::gly_enrich_reactome, sig_exp, !!!call_args)
      )
      ctx <- ctx_add_table(
        ctx,
        kind,
        glystats::get_tidy_result(enrich_res),
        paste0(toupper(kind), " enrichment analysis results.")
      )
      p <- glyvis::plot_enrich(enrich_res, type = plot_type)
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
#' Advanced glycan structure analysis that summarizes structural properties of a glycome or each glycosite.
#' Need glycan structure information.
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
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step by default if the experiment has glycan structures.
#' - After this step, it should be followed by the DEA and visualization steps.
#'
#' @inheritParams glydet::derive_traits
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_derive_traits()
#' @seealso [glydet::derive_traits()]
#' @export
step_derive_traits <- function(trait_fns = NULL, mp_fns = NULL, mp_cols = NULL) {
  signature <- rlang::expr_deparse(match.call())
  step(
    id = "derive_traits",
    label = "Derived trait calculation",
    condition = function(ctx) {
      check <- .has_glycan_structure(ctx_get_data(ctx, "exp"))
      reason <- "glycan structures are not available in the experiment"
      list(check = check, reason = reason)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      trait_exp <- glydet::derive_traits(
        exp,
        trait_fns = trait_fns,
        mp_fns = mp_fns,
        mp_cols = mp_cols
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
      msg <- paste0(
        "Derived traits were calculated. ",
        "Number of derived traits: ", length(unique(tbl$trait)), "."
      )
      trait_definition_tbl <- tbl |>
        dplyr::distinct(.data$trait, .data$explanation)
      definition_parts <- paste0("- ", trait_definition_tbl$trait, ": ", trait_definition_tbl$explanation)
      definition_parts <- paste(definition_parts, collapse = "\n")
      msg <- paste0(msg, "\n\n", "Trait definitions:\n\n", definition_parts)
      msg
    },
    generate = "trait_exp",
    require = "exp",
    signature = signature
  )
}

#' Step: Quantify Motifs
#'
#' Quantify glycan motifs using `glydet::quantify_motifs()`.
#' The motifs are extracted using `glymotif::extract_branch_motif()` for N-glycans
#' and `glymotif::extract_motif()` for others.
#' Advanced glycan motif analysis that quantify glycan motifs (substructures) of a glycome or each glycosite.
#' Need glycan structure information.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to quantify motifs for
#'
#' Data generated:
#' - `motif_exp`: The experiment with quantified motifs
#'
#' Tables generated:
#' - `quantified_motifs`: A table containing the quantified motifs.
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#' - This step should be followed by the DEA step and visualization steps.
#'
#' @param max_size Maximum size of motifs to extract.
#' @param method Method for motif quantification ("relative" or "absolute").
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_quantify_motifs()
#' @seealso [glydet::quantify_motifs()], [glymotif::extract_motif()], [glymotif::extract_branch_motif()]
#' @export
step_quantify_motifs <- function(max_size = 3, method = "relative") {
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "quantify_motifs",
    label = "Motif quantification",
    condition = function(ctx) {
      check <- .has_glycan_structure(ctx_get_data(ctx, "exp"))
      reason <- "glycan structures are not available in the experiment"
      list(check = check, reason = reason)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      type <- glyexp::get_glycan_type(exp)

      if (type == "N") {
        motifs <- glymotif::extract_branch_motif(exp$var_info$glycan_structure)
        alignment <- "exact"
      } else {
        motifs <- glymotif::extract_motif(exp$var_info$glycan_structure, max_size = max_size)
        alignment <- "substructure"
      }

      motif_exp <- glydet::quantify_motifs(
        exp,
        motifs = motifs,
        method = method,
        alignments = alignment,
        ignore_linkages = FALSE
      )

      ctx <- ctx_add_data(ctx, "motif_exp", motif_exp)
      ctx <- ctx_add_table(ctx, "quantified_motifs", tibble::as_tibble(motif_exp), "Motif quantification results.")
      ctx
    },
    report = function(x) {
      tbl <- x$tables[["quantified_motifs"]]
      n_motifs <- length(unique(tbl$motif))
      motif_type_msg <- if (glyexp::get_glycan_type(x$exp) == "N") {
        "Branching motifs for N-glycans were extracted. "
      } else {
        "All motifs were extracted. "
      }
      msg <- paste0(
        "Motif quantification was performed. ",
        motif_type_msg,
        "Number of quantified motifs: ", n_motifs, "."
      )
      msg
    },
    generate = "motif_exp",
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
#' - `motif_heatmap`: A heatmap plot (if `on = "motif_exp"`)
#' - `sig_motif_heatmap`: A heatmap plot (if `on = "sig_motif_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if needed.
#' - It is recommended to use this step on significant results (e.g. `on = "sig_exp"`) if available.
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
#'   Default is "exp".
#' @param ... Additional arguments passed to [glyvis::plot_heatmap()].
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_heatmap()
#' step_heatmap(on = "sig_exp")
#' step_heatmap(on = "trait_exp")
#' @seealso [glyvis::plot_heatmap()]
#' @export
step_heatmap <- function(on = "exp", ...) {
  rlang::check_installed("pheatmap")
  rlang::check_installed("ggplotify")
  signature <- rlang::expr_deparse(match.call())

  on_meta <- .resolve_on(on)
  plot_name <- paste0("heatmap", on_meta$id_suffix)
  label <- paste0("Heatmap", on_meta$label_suffix)

  step(
    id = paste0("heatmap", on_meta$id_suffix),
    label = label,
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      p <- glyvis::plot_heatmap(exp, ...)
      ctx_add_plot(ctx, plot_name, p, paste0("Heatmap of ", on, "."))
    },
    require = on,
    signature = signature
  )
}

#' Step: Logo Plot
#'
#' Create a logo plot for glycosylation sites using `glyvis::plot_logo()`.
#' The logo plot visualizes the amino acid sequence patterns around glycosylation sites.
#' This step is only applicable for glycoproteomics experiments.
#'
#' @details
#' Data required:
#' - Depends on `on` parameter (default: `exp`)
#'
#' Plots generated:
#' - `logo`: A logo plot (if `on = "exp"`)
#' - `sig_logo`: A logo plot (if `on = "sig_exp"`)
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if the user explicitly asks for logo plot.
#'
#' @param on Name of the experiment data in `ctx$data` to plot.
#'   One of "exp", "sig_exp". Default is "exp".
#' @inheritParams glyvis::plot_logo
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_logo()
#' step_logo(fasta = "proteins.fasta")
#' step_logo(on = "sig_exp")
#' @seealso [glyvis::plot_logo()]
#' @export
step_logo <- function(on = "exp", n_aa = 5L, fasta = NULL, ...) {
  rlang::check_installed("ggseqlogo")
  checkmate::assert_choice(on, c("exp", "sig_exp"))
  signature <- rlang::expr_deparse(match.call())

  on_meta <- .resolve_on(on)
  plot_name <- paste0("logo", on_meta$id_suffix)
  label <- paste0("Logo plot", on_meta$label_suffix)

  step(
    id = paste0("logo", on_meta$id_suffix),
    label = label,
    condition = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      if (glyexp::get_exp_type(exp) != "glycoproteomics") {
        return(list(check = FALSE, reason = "logo plot is only applicable for glycoproteomics experiments"))
      }
      list(check = TRUE, reason = NULL)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, on)
      p <- glyvis::plot_logo(exp, n_aa = n_aa, fasta = fasta, ...)
      ctx_add_plot(ctx, plot_name, p, paste0("Logo plot of glycosylation sites for ", on, "."))
    },
    require = on,
    signature = signature
  )
}

#' Step: ROC Analysis
#'
#' Perform ROC analysis using `glystats::gly_roc()`,
#' extract top 10 variables with highest AUC,
#' and plot ROC curves for these variables using `glyvis::plot_roc()`.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to perform ROC analysis on
#'
#' Tables generated:
#' - `roc_auc`: A table containing the ROC AUC values for all variables
#'
#' Plots generated:
#' - `roc_curves`: ROC curves for the top 10 variables
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if the user explicitly asks for ROC analysis,
#'   or if he/she mentions "biomarker(s)" in the prompt.
#'
#' @inheritParams glystats::gly_roc
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_roc()
#' @seealso [glystats::gly_roc()], [glyvis::plot_roc()]
#' @export
step_roc <- function(pos_class = NULL) {
  rlang::check_installed("pROC")
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "roc",
    label = "ROC analysis",
    condition = function(ctx) {
      if (length(unique(ctx_get_data(ctx, "exp")$sample_info$group)) == 2) {
        list(check = TRUE, reason = NULL)
      } else {
        list(check = FALSE, reason = "more than 2 groups are in the experiment")
      }
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      roc_res <- glystats::gly_roc(exp, pos_class = pos_class)

      # Save full AUC results
      ctx <- ctx_add_table(
        ctx,
        "roc_auc",
        roc_res$tidy_result$auc,
        "ROC AUC values for all variables."
      )

      # Extract top 10 variables
      top_vars <- roc_res$tidy_result$auc |>
        dplyr::slice_max(.data$auc, n = 10, with_ties = FALSE) |>
        dplyr::pull("variable")

      # Filter experiment and plot
      sub_exp <- exp |>
        glyexp::filter_var(.data$variable %in% top_vars)

      p_roc <- glyvis::plot_roc(sub_exp, type = "roc")

      ctx <- ctx_add_plot(
        ctx,
        "roc_curves",
        p_roc,
        "ROC curves for top 10 variables."
      )

      ctx
    },
    report = function(x) {
      auc_vals <- x$tables$roc_auc$auc

      n_gt_0.8 <- sum(auc_vals > 0.8, na.rm = TRUE)
      n_gt_0.9 <- sum(auc_vals > 0.9, na.rm = TRUE)
      max_auc <- max(auc_vals, na.rm = TRUE)

      glue::glue(
        "ROC analysis was performed. ",
        "There are {n_gt_0.8} variables with AUC > 0.8, ",
        "{n_gt_0.9} variables with AUC > 0.9. ",
        "The highest AUC is {round(max_auc, 3)}."
      )
    },
    require = "exp",
    signature = signature
  )
}

.on_choices <- function() {
  c("exp", "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp")
}

.has_glycan_structure <- function(exp) {
  "glycan_structure" %in% colnames(exp$var_info)
}

#' Resolve properties from 'on' parameter
#'
#' @param on Name of the experiment data.
#' @returns A list with suffixes and prefixes.
#' @noRd
.resolve_on <- function(on) {
  checkmate::assert_choice(on, .on_choices())
  list(
    id_suffix = switch(on,
      exp = "",
      sig_exp = "_sig",
      trait_exp = "_trait",
      sig_trait_exp = "_sig_trait",
      motif_exp = "_motif",
      sig_motif_exp = "_sig_motif"
    ),
    label_suffix = switch(on,
      exp = "",
      sig_exp = " of significant variables",
      trait_exp = " of traits",
      sig_trait_exp = " of significant traits",
      motif_exp = " of motifs",
      sig_motif_exp = " of significant motifs"
    )
  )
}
