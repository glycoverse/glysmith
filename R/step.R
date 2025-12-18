#' Create a GlySmith step object
#'
#' A step is a function object with metadata that mutates and returns context.
#'
#' @param id Step id.
#' @param label Human-readable label for progress output.
#' @param run A function(ctx) that returns updated ctx.
#' @param report A function(x) that returns a markdown string for reporting, or NULL.
#' @param require Character vector of required `ctx$data` keys.
#' @param generate Character vector of generated `ctx$data` keys.
#' @param condition Optional function(ctx) returning TRUE/FALSE to decide execution.
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
  retry = 0L
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
      retry = retry
    ),
    class = "glysmith_step"
  )
}

#' @export
print.glysmith_step <- function(x, ...) {
  cli::cli_text("<step {.val {x$id}}> {.emph {x$label}}")
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
    step_dea(),
    step_volcano(),
    step_sig_enrich_go(),
    step_sig_enrich_kegg(),
    step_sig_enrich_reactome(),
    step_derive_traits(),
    step_dta()
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
    require = "exp",
    generate = "raw_exp"
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
    require = "exp"
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
    report = function(x) {
      eig <- x$tables[["pca_eigenvalues"]]
      "PCA was performed and the results were saved in `plots$pca` and `tables$pca_*`."
    },
    require = "exp"
  )
}

#' Step: Differential Expression Analysis (DEA)
#'
#' Run differential analysis using `glystats::gly_limma()`.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to run DEA on
#'
#' Data generated:
#' - `dea_res`: The DEA results from `glystats::gly_limma()`
#'
#' Tables generated:
#' - `dea`: A table containing the DEA results
#'
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_dea(glystats.gly_limma.p_adj_method = "BH")`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dea()
#' @seealso [glystats::gly_limma()]
#' @export
step_dea <- function(...) {
  step_dots <- rlang::list2(...)
  step(
    id = "dea",
    label = "Differential expression analysis",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      dea_res <- .run_function(
        glystats::gly_limma,
        exp,
        step_id = "dea",
        global_dots = ctx$dots,
        step_dots = step_dots
      )
      ctx$data$dea_res <- dea_res
      ctx_add_table(
        ctx,
        "dea",
        glystats::get_tidy_result(dea_res),
        "Differential expression analysis results of all comparisons for all variables."
      )
    },
    report = function(x) {
      tbl <- x$tables[["dea"]]
      sig <- length(unique(tbl$variable[tbl$p_adj < 0.05]))
      msg <- "Differential expression analysis was performed and the results were saved in `tables$dea`. "
      if (sig > 0) {
        msg <- paste0(msg, "Number of significant items (FDR/adjusted p < 0.05): ", sig, ".\n")
      } else {
        msg <- paste0(msg, "No significant items (FDR/adjusted p < 0.05). \n")
      }
      msg
    },
    generate = "dea_res",
    require = "exp"
  )
}

#' Step: Volcano Plot
#'
#' Create a volcano plot from DEA results using `glyvis::plot_volcano()`.
#' This step requires [step_dea()].
#' Currently only supports experiments with two groups.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to create a volcano plot for
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
  step_dots <- rlang::list2(...)
  step(
    id = "volcano",
    label = "Volcano plot",
    run = function(ctx) {
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
    },
    report = function(x) {
      "This step will generate a volcano plot for each contrast in the DEA results."
    },
    require = "dea_res"
  )
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
#' This step requires [step_dea()].
#' Only execute for glycoproteomics experiments.
#' Use all genes in OrgDb as the background.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to perform GO enrichment analysis for
#' - `dea_res`: The DEA results from `glystats::gly_limma()`
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
  step_sig_enrich("go", universe = universe, ...)
}

#' Step: KEGG Enrichment Analysis on Differentially Expressed Variables
#'
#' Perform KEGG enrichment analysis on differentially expressed variables using `glystats::gly_enrich_kegg()`.
#' This step requires [step_dea()].
#' Only execute for glycoproteomics experiments.
#' Use all genes in OrgDb as the background.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to perform KEGG enrichment analysis for
#' - `dea_res`: The DEA results from `glystats::gly_limma()`
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
  step_sig_enrich("kegg", universe = universe, retry = 2L, ...)
}

#' Step: Reactome Enrichment Analysis on Differentially Expressed Variables
#'
#' Perform Reactome enrichment analysis on differentially expressed variables using `glystats::gly_enrich_reactome()`.
#' This step requires [step_dea()].
#' Only execute for glycoproteomics experiments.
#' Use all genes in OrgDb as the background.
#'
#' @details
#' Data required:
#' - `exp`: The experiment to perform Reactome enrichment analysis for
#' - `dea_res`: The DEA results from `glystats::gly_limma()`
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
  step_sig_enrich("reactome", universe = universe, retry = 2L, ...)
}

#' Step: Enrichment Analysis on Differentially Expressed Variables
#'
#' @description
#' Run functional enrichment analysis on differentially expressed variables using one of:
#' - `glystats::gly_enrich_go()`
#' - `glystats::gly_enrich_kegg()`
#' - `glystats::gly_enrich_reactome()`
#'
#' This step requires [step_dea()].
#' Only execute for glycoproteomics experiments.
#' Use all genes in OrgDb as the background.
#'
#' @param kind Enrichment type: `"go"`, `"kegg"`, or `"reactome"`.
#' @param universe The universe (background) to use for enrichment analysis.
#'   One of "all" (all genes in OrgDb), "detected" (detected variables in `exp`).
#' @param retry Number of retries if the step errors.
#' @noRd
step_sig_enrich <- function(kind = c("go", "kegg", "reactome"), universe = c("all", "detected"), retry = 0L, ...) {
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
    condition = function(ctx) glyexp::get_exp_type(ctx_get_data(ctx, "exp")) == "glycoproteomics",
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      sig_exp <- glystats::filter_sig_vars(exp, ctx$data$dea_res)
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
      msg <- paste0("Enrichment analysis was performed on differentially expressed variables and the results were saved in `tables$", kind, "` and `plots$", kind, "`.")
      if (n_sig > 0) {
        msg <- paste0(msg, " Number of significant items (FDR/adjusted p < 0.05): ", n_sig, ".\n\n")
        msg <- paste0(msg, "Top terms: \n\n", paste("- ", tbl$description[1:min(5, n_sig)], collapse = "\n"), "\n")
      } else {
        msg <- paste0(msg, " No significant items (FDR/adjusted p < 0.05).\n")
      }
      msg
    },
    require = c("dea_res", "exp"),
    retry = retry
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
  step_dots <- rlang::list2(...)
  step(
    id = "derive_traits",
    label = "Derived trait calculation",
    condition = function(ctx) "glycan_structure" %in% colnames(ctx_get_data(ctx, "exp")$var_info),
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
    require = "exp"
  )
}

#' Step: Differential Trait Analysis (DTA)
#'
#' Run differential analysis on derived traits using `glystats::gly_limma()`.
#' This step requires [step_derive_traits()].
#'
#' @details
#' Data required:
#' - `trait_exp`: The experiment with derived traits
#'
#' Tables generated:
#' - `dta_res`: A table containing the differential trait analysis results.
#'
#' @param ... Step-specific arguments passed to underlying functions.
#'   Use the format `pkg.func.arg`.
#'   For example, `step_dta(glystats.gly_limma.p_adj_method = "BH")`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_dta()
#' @seealso [glystats::gly_limma()]
#' @export
step_dta <- function(...) {
  step_dots <- rlang::list2(...)
  step(
    id = "dta",
    label = "Differential trait analysis",
    condition = function(ctx) "glycan_structure" %in% colnames(ctx_get_data(ctx, "exp")$var_info),
    run = function(ctx) {
      trait_exp <- ctx$data$trait_exp
      if (is.null(trait_exp)) {
        cli::cli_abort(c(
          "Missing required ctx$data for this step.",
          "x" = "Step 'dta' requires {.field trait_exp}.",
          "i" = "Add {.fn step_derive_traits} before {.fn step_dta} in the blueprint."
        ))
      }
      filtered_trait_exp <- glyclean::remove_constant(trait_exp)
      dta_res <- .run_function(
        glystats::gly_limma,
        filtered_trait_exp,
        step_id = "dta",
        global_dots = ctx$dots,
        step_dots = step_dots
      )
      ctx_add_table(ctx, "dta", glystats::get_tidy_result(dta_res), "Differential trait analysis results.")
    },
    report = function(x) {
      tbl <- x$tables[["dta"]]
      sig <- length(unique(tbl$variable[tbl$p_adj < 0.05]))
      msg <- "Differential trait analysis was performed and the results were saved in `tables$dta`. "
      if (glyexp::get_exp_type(ctx_get_data(ctx, "exp")) == "glycomics") {
        item_name <- "traits"
      } else {
        item_name <- "site-specific traits"
      }
      if (sig > 0) {
        msg <- paste0(msg, "Number of significant ", item_name, " (FDR/adjusted p < 0.05): ", sig, ".\n")
      } else {
        msg <- paste0(msg, "No significant ", item_name, " (FDR/adjusted p < 0.05).\n")
      }
      msg
    },
    require = "trait_exp"
  )
}
