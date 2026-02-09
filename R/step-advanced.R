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
step_derive_traits <- function(
  trait_fns = NULL,
  mp_fns = NULL,
  mp_cols = NULL
) {
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
      ctx <- ctx_add_table(
        ctx,
        "derived_traits",
        tibble::as_tibble(trait_exp),
        "Derived trait calculation results."
      )
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
        "Number of derived traits: ",
        length(unique(tbl$trait)),
        "."
      )
      trait_definition_tbl <- tbl |>
        dplyr::distinct(.data$trait, .data$explanation)
      definition_parts <- paste0(
        "- ",
        trait_definition_tbl$trait,
        ": ",
        trait_definition_tbl$explanation
      )
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
#' The motifs are extracted using `glymotif::branch_motifs()` for N-glycans
#' and `glymotif::dynamic_motifs()` for others.
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
#' @seealso [glydet::quantify_motifs()], [glymotif::dynamic_motifs()], [glymotif::branch_motifs()]
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
        motifs <- glymotif::branch_motifs()
      } else {
        motifs <- glymotif::dynamic_motifs()
      }

      motif_exp <- glydet::quantify_motifs(
        exp,
        motifs = motifs,
        method = method,
        ignore_linkages = FALSE
      )

      ctx <- ctx_add_data(ctx, "motif_exp", motif_exp)
      ctx <- ctx_add_table(
        ctx,
        "quantified_motifs",
        tibble::as_tibble(motif_exp),
        "Motif quantification results."
      )
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
        "Number of quantified motifs: ",
        n_motifs,
        "."
      )
      msg
    },
    generate = "motif_exp",
    require = "exp",
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
#' - If the experiment has more than 2 groups but the user wants a specific two-group
#'   comparison, ask which two groups to compare and include
#'   `step_subset_groups(groups = c("A", "B"))` before this step.
#'
#' @param plot_width Width of the plot in inches. Default is 5.
#' @param plot_height Height of the plot in inches. Default is 5.
#' @inheritParams glystats::gly_roc
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_roc()
#' @seealso [glystats::gly_roc()], [glyvis::plot_roc()]
#' @export
step_roc <- function(pos_class = NULL, plot_width = 5, plot_height = 5) {
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
        "ROC curves for top 10 variables.",
        width = plot_width,
        height = plot_height
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
