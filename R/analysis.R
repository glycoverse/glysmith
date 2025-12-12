#' Forge an Analysis for Group Comparison
#'
#' @description
#' This function performs comprehensive analysis for group comparison.
#'
#' The pipeline includes:
#' - Preprocessing using `glyclean::auto_clean()`
#' - Identification overview using `glyexp::summarize_experiment()`
#' - Principal component analysis (PCA) with `glystats::gly_pca()` and `glyvis::plot_pca()`
#' - Differential expression analysis (DEA) with `glystats::gly_limma()` and `glyvis::plot_volcano()`
#'
#' If experiment type is "glycoproteomics", the pipeline will also include:
#' - Functional enrichment analysis using `glystats::gly_enrich_go()`, `glystats::gly_enrich_kegg()`,
#'   and `glystats::gly_enrich_reactome()`, as well as plotting with `glyvis::plot_enrich()`
#'
#' If glycan structure is available and glycan type is "N",
#' the pipeline will also include:
#' - Derived trait calculation using `glydet::derive_traits()`
#' - Differential trait analysis (DTA) with `glystats::gly_limma()`
#'
#' @param exp A `glyexp::experiment()` object.
#' @param group_col Column name of group information in the sample information.
#'   Used for various analyses. Default is "group".
#' @param ... Additional arguments passed to the functions.
#'   Use the format `pkg.func.arg` to pass arguments to the functions.
#'   For example, if you want to pass argument `p_adj_method = "BH"` to `glystats::gly_limma()`,
#'   set `glystats.gly_limma.p_adj_method = "BH"`.
#'   Note that arguments about group column specification is controlled by `group_col` argument,
#'   and should not be passed to `...`.
#'
#' @returns A `glysmith_result` object, with the following components:
#'   - `exp`: the experiment after preprocessing.
#'   - `plots`: a named list of ggplot objects.
#'   - `tables`: a named list of tibbles.
#'   - `meta`: a named list of metadata. Currently two elements:
#'     - `explanation`: a named character vector or list of explanations for each plot and table,
#'        with keys like `tables$summary` and `plots$pca`.
#'     - `steps`: a character vector of the steps of the analysis.
#'
#' @examples
#' exp <- glyexp::real_experiment
#' result <- forge_analysis(exp)
#' print(result)
#'
#' @export
forge_analysis <- function(exp, group_col = "group", ...) {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col)
  if (!group_col %in% colnames(exp$sample_info)) {
    cli::cli_abort("Column name '{group_col}' is not found in the sample information.")
  }
  exp$sample_info[[group_col]] <- droplevels(as.factor(exp$sample_info[[group_col]]))

  dots <- rlang::list2(...)
  ctx <- new_ctx(exp, group_col, dots)

  steps <- list(
    step_preprocess(),
    step_ident_overview(),
    step_pca(),
    step_dea(),
    step_volcano(),
    step_enrich("go"),
    step_enrich("kegg"),
    step_enrich("reactome"),
    step_derive_traits(),
    step_dta()
  )

  ctx <- run_recipe(ctx, steps)

  glysmith_result(exp = ctx$exp, plots = ctx$plots, tables = ctx$tables, meta = ctx$meta)
}

# --- Steps --------------------------------------------------------------------

#' @noRd
step_preprocess <- function() {
  step(
    id = "preprocessing",
    label = "Preprocessing",
    run = function(ctx) {
      ctx$exp <- .run_function(glyclean::auto_clean, ctx$exp, ctx$group_col, ctx$dots, "group_col")
      ctx
    },
    outputs = list()
  )
}

#' @noRd
step_ident_overview <- function() {
  step(
    id = "identification_overview",
    label = "Identification overview",
    run = function(ctx) {
      tbl <- .run_function(glyexp::summarize_experiment, ctx$exp, ctx$group_col, ctx$dots)
      ctx_add_table(ctx, "summary", tbl, "Identification overview of the experiment.")
    },
    outputs = list(tables = "summary")
  )
}

#' @noRd
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
    outputs = list(
      tables = c("pca_samples", "pca_variables", "pca_eigenvalues"),
      plots = "pca"
    )
  )
}

#' @noRd
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
    outputs = list(tables = "dea")
  )
}

#' @noRd
step_volcano <- function() {
  step(
    id = "volcano",
    label = "Volcano plot",
    condition = function(ctx) {
      g <- ctx$exp$sample_info[[ctx$group_col]]
      length(levels(g)) == 2
    },
    run = function(ctx) {
      dea_res <- rlang::`%||%`(
        ctx$data$dea_res,
        .run_function(glystats::gly_limma, ctx$exp, ctx$group_col, ctx$dots, "group_col")
      )
      p <- .run_function(glyvis::plot_volcano, dea_res, ctx$group_col, ctx$dots)
      ctx_add_plot(ctx, "volcano", p, "Volcano plot for the comparison of the two groups.")
    },
    outputs = list(plots = "volcano")
  )
}

#' @noRd
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
    outputs = list(tables = kind, plots = kind)
  )
}

#' @noRd
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
    outputs = list(tables = "derived_traits")
  )
}

#' @noRd
step_dta <- function() {
  step(
    id = "dta",
    label = "Differential trait analysis",
    condition = function(ctx) "glycan_structure" %in% colnames(ctx$exp$var_info),
    run = function(ctx) {
      trait_exp <- rlang::`%||%`(
        ctx$data$trait_exp,
        .run_function(glydet::derive_traits, ctx$exp, ctx$group_col, ctx$dots)
      )
      suppressMessages(filtered_trait_exp <- glyclean::remove_constant(trait_exp))
      dta_res <- .run_function(glystats::gly_limma, filtered_trait_exp, ctx$group_col, ctx$dots, "group_col")
      ctx_add_table(ctx, "dta", glystats::get_tidy_result(dta_res), "Differential trait analysis results.")
    },
    outputs = list(tables = "dta")
  )
}
