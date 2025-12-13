#' Create a GlySmith step object
#'
#' A step is a function object with metadata that mutates and returns context.
#'
#' @param id Step id.
#' @param label Human-readable label for progress output.
#' @param run A function(ctx) that returns updated ctx.
#' @param report A function(x) that returns a markdown string for reporting, or NULL.
#' @param outputs A list describing declared outputs (tables/plots/meta).
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
  condition = NULL
) {
  structure(
    list(
      id = id,
      label = label,
      run = run,
      report = report,
      outputs = outputs,
      require = require,
      generate = generate,
      condition = condition
    ),
    class = "glysmith_step"
  )
}

step_preprocess <- function() {
  step(
    id = "preprocessing",
    label = "Preprocessing",
    run = function(ctx) {
      ctx$exp <- .run_function(glyclean::auto_clean, ctx$exp, ctx$group_col, ctx$dots, "group_col")
      ctx
    },
    report = NULL,
    outputs = list(),
    require = character(0),
    generate = character(0)
  )
}

step_ident_overview <- function() {
  step(
    id = "identification_overview",
    label = "Identification overview",
    run = function(ctx) {
      tbl <- .run_function(glyexp::summarize_experiment, ctx$exp, ctx$group_col, ctx$dots)
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
    outputs = list(tables = "summary"),
    require = character(0),
    generate = character(0)
  )
}

step_pca <- function() {
  step(
    id = "pca",
    label = "Principal component analysis",
    run = function(ctx) {
      pca_res <- .run_function(glystats::gly_pca, ctx$exp, ctx$group_col, ctx$dots)
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
      p <- .run_function(glyvis::plot_pca, pca_res, ctx$group_col, ctx$dots, "group_col")
      ctx_add_plot(ctx, "pca", p, "PCA plot colored by group.")
    },
    report = function(x) {
      eig <- x$tables[["pca_eigenvalues"]]
      "PCA was performed and the results were saved in `plots$pca` and `tables$pca_*`."
    },
    outputs = list(
      tables = c("pca_samples", "pca_variables", "pca_eigenvalues"),
      plots = "pca"
    ),
    require = character(0),
    generate = character(0)
  )
}

step_dea <- function() {
  step(
    id = "dea",
    label = "Differential expression analysis",
    run = function(ctx) {
      dea_res <- .run_function(glystats::gly_limma, ctx$exp, ctx$group_col, ctx$dots, "group_col")
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
    outputs = list(tables = "dea"),
    require = character(0),
    generate = "dea_res"
  )
}

step_volcano <- function() {
  step(
    id = "volcano",
    label = "Volcano plot",
    condition = function(ctx) {
      g <- ctx$exp$sample_info[[ctx$group_col]]
      length(levels(g)) == 2
    },
    run = function(ctx) {
      dea_res <- ctx$data$dea_res
      if (is.null(dea_res)) {
        cli::cli_abort(c(
          "Missing required ctx$data for this step.",
          "x" = "Step 'volcano' requires {.field dea_res}.",
          "i" = "Add {.fn step_dea} before {.fn step_volcano} in the blueprint."
        ))
      }
      p <- .run_function(glyvis::plot_volcano, dea_res, ctx$group_col, ctx$dots)
      ctx_add_plot(ctx, "volcano", p, "Volcano plot for the comparison of the two groups.")
    },
    report = function(x) {
      "When the comparison only contains two groups, this step will generate a volcano plot in `plots$volcano`."
    },
    outputs = list(plots = "volcano"),
    require = "dea_res",
    generate = character(0)
  )
}

step_enrich_go <- function() {
  step_enrich("go")
}

step_enrich_kegg <- function() {
  step_enrich("kegg")
}

step_enrich_reactome <- function() {
  step_enrich("reactome")
}

step_enrich <- function(kind = c("go", "kegg", "reactome")) {
  kind <- match.arg(kind)
  f <- switch(
    kind,
    go = glystats::gly_enrich_go,
    kegg = glystats::gly_enrich_kegg,
    reactome = glystats::gly_enrich_reactome
  )
  label <- paste0(toupper(kind), " enrichment analysis")

  step(
    id = paste0("enrich_", kind),
    label = label,
    condition = function(ctx) glyexp::get_exp_type(ctx$exp) == "glycoproteomics",
    run = function(ctx) {
      sig_exp <- glystats::filter_sig_vars(ctx$exp, ctx$data$dea_res)
      enrich_res <- .run_function(f, ctx$exp, ctx$group_col, ctx$dots)
      ctx <- ctx_add_table(
        ctx,
        kind,
        glystats::get_tidy_result(enrich_res),
        paste0(toupper(kind), " enrichment analysis results.")
      )
      p <- .run_function(glyvis::plot_enrich, enrich_res, ctx$group_col, ctx$dots)
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
    outputs = list(tables = kind, plots = kind),
    require = "dea_res",
    generate = character(0)
  )
}

step_derive_traits <- function() {
  step(
    id = "derive_traits",
    label = "Derived trait calculation",
    condition = function(ctx) "glycan_structure" %in% colnames(ctx$exp$var_info),
    run = function(ctx) {
      trait_exp <- .run_function(glydet::derive_traits, ctx$exp, ctx$group_col, ctx$dots)
      ctx$data$trait_exp <- trait_exp
      ctx_add_table(ctx, "derived_traits", tibble::as_tibble(trait_exp), "Derived trait calculation results.")
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
    outputs = list(tables = "derived_traits"),
    require = character(0),
    generate = "trait_exp"
  )
}

step_dta <- function() {
  step(
    id = "dta",
    label = "Differential trait analysis",
    condition = function(ctx) "glycan_structure" %in% colnames(ctx$exp$var_info),
    run = function(ctx) {
      trait_exp <- ctx$data$trait_exp
      if (is.null(trait_exp)) {
        cli::cli_abort(c(
          "Missing required ctx$data for this step.",
          "x" = "Step 'dta' requires {.field trait_exp}.",
          "i" = "Add {.fn step_derive_traits} before {.fn step_dta} in the blueprint."
        ))
      }
      suppressMessages(filtered_trait_exp <- glyclean::remove_constant(trait_exp))
      dta_res <- .run_function(glystats::gly_limma, filtered_trait_exp, ctx$group_col, ctx$dots, "group_col")
      ctx_add_table(ctx, "dta", glystats::get_tidy_result(dta_res), "Differential trait analysis results.")
    },
    report = function(x) {
      tbl <- x$tables[["dta"]]
      sig <- length(unique(tbl$variable[tbl$p_adj < 0.05]))
      msg <- "Differential trait analysis was performed and the results were saved in `tables$dta`. "
      if (glyexp::get_exp_type(x$exp) == "glycomics") {
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
    outputs = list(tables = "dta"),
    require = "trait_exp",
    generate = character(0)
  )
}