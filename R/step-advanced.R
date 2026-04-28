#' Step: Infer Glycan Structures
#'
#' @description
#' Infer glycan structures from the `glycan_composition` column in `var_info`.
#' This step uses `glyanno::comp_to_struc()` with a structure database from
#' `glydb::glydb_structures()` and keeps only variables with an inferred
#' structure.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment whose glycan structures should be inferred
#'
#' Data generated:
#' - `uninferred_exp`: The original experiment before structure inference
#'
#' Tables generated:
#' - `inferred_structures`: A table containing the inferred structure for each
#'   original variable and whether inference succeeded.
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step when the user requests structure-aware analysis but the
#'   experiment has glycan compositions and no glycan structures.
#' - This step should be placed before [step_derive_traits()],
#'   [step_quantify_dynamic_motifs()], or [step_quantify_branch_motifs()].
#' - Mention that variables without inferred structures are removed.
#'
#' @param species Species name used to restrict the glycan structure database.
#'   Default is `NULL`, which does not restrict by species.
#' @param structure_level Structure level passed to [glydb::glydb_structures()].
#'   One of `"intact"`, `"topological"`, or `"basic"`.
#'   Default is `"topological"`.
#' @param db Optional glycan structure database. If `NULL`, the database is
#'   created with [glydb::glydb_structures()] using `species`,
#'   `structure_level`, and the experiment's glycan type.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_infer_structure()
#' step_infer_structure(species = "Homo sapiens")
#' @seealso [glyanno::comp_to_struc()], [glydb::glydb_structures()]
#' @export
step_infer_structure <- function(
  species = NULL,
  structure_level = "topological",
  db = NULL
) {
  checkmate::assert_string(species, null.ok = TRUE)
  checkmate::assert_choice(structure_level, c("intact", "topological", "basic"))

  signature <- rlang::expr_deparse(match.call())

  step(
    id = "infer_structure",
    label = "Glycan structure inference",
    condition = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      if (.has_glycan_structure(exp)) {
        return(list(
          check = FALSE,
          reason = "glycan structures are already available in the experiment"
        ))
      }
      if (!"glycan_composition" %in% colnames(exp$var_info)) {
        return(list(
          check = FALSE,
          reason = "glycan compositions are not available in the experiment"
        ))
      }
      list(check = TRUE, reason = NULL)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      db_to_use <- db %||% glydb::glydb_structures(
        structure_level = structure_level,
        species = species,
        glycan_type = glyexp::get_glycan_type(exp)
      )

      comps <- exp$var_info$glycan_composition
      if (glyrepr::get_mono_type(db_to_use) == "generic") {
        comps <- glyrepr::convert_to_generic(comps)
      }

      inferred_structures <- glyanno::comp_to_struc(
        comps,
        db = db_to_use,
        return_best = TRUE
      )
      inference_tbl <- exp$var_info |>
        dplyr::mutate(
          glycan_structure = inferred_structures,
          matched = !is.na(.data$glycan_structure)
        )

      inferred_exp <- exp
      inferred_exp$var_info$glycan_structure <- inferred_structures
      inferred_exp <- inferred_exp |>
        glyexp::filter_var(!is.na(.data$glycan_structure))

      ctx <- ctx_add_data(ctx, "exp", inferred_exp)
      ctx <- ctx_add_data(ctx, "uninferred_exp", exp)
      ctx <- ctx_add_table(
        ctx,
        "inferred_structures",
        inference_tbl,
        "Glycan structure inference results."
      )
      ctx
    },
    report = function(x) {
      tbl <- x$tables[["inferred_structures"]]
      n_total <- nrow(tbl)
      n_matched <- sum(tbl$matched)
      n_removed <- n_total - n_matched
      paste0(
        "Glycan structures were inferred from glycan compositions. ",
        "Matched variables: ",
        n_matched,
        " of ",
        n_total,
        ". ",
        "Variables without inferred structures removed: ",
        n_removed,
        "."
      )
    },
    require = "exp",
    generate = "uninferred_exp",
    signature = signature
  )
}

#' Step: Derived Trait Calculation
#'
#' @description
#' Calculate glycan derived traits using `glydet::derive_traits()`.
#' Advanced glycan structure analysis that summarizes structural properties of a glycome or each glycosite.
#' Need glycan structure information.
#'
#' This step requires `exp` (experiment data).
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

#' Step: Quantify Dynamic Motifs
#'
#' @description
#' Quantify glycan motifs using `glydet::quantify_motifs()` with `glymotif::dynamic_motifs()`.
#' This extracts all possible motifs from glycan structures.
#' Works with any glycan type.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment to quantify motifs for
#'
#' Data generated:
#' - `dynamic_motif_exp`: The experiment with quantified motifs
#'
#' Tables generated:
#' - `dynamic_motifs`: A table containing the quantified motifs.
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if motif analysis is needed for non-N-glycans or when comprehensive motif extraction is desired.
#' - This step should be followed by DEA and visualization steps.
#'
#' @param max_size Maximum size of motifs to extract. Default is 3.
#' @param method Method for motif quantification ("relative" or "absolute"). Default is "relative".
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_quantify_dynamic_motifs()
#' @seealso [glydet::quantify_motifs()], [glymotif::dynamic_motifs()]
#' @export
step_quantify_dynamic_motifs <- function(max_size = 3, method = "relative") {
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "quantify_dynamic_motifs",
    label = "Dynamic motif quantification",
    condition = function(ctx) {
      check <- .has_glycan_structure(ctx_get_data(ctx, "exp"))
      reason <- "glycan structures are not available in the experiment"
      list(check = check, reason = reason)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      motifs <- glymotif::dynamic_motifs(max_size = max_size)

      dynamic_motif_exp <- glydet::quantify_motifs(
        exp,
        motifs = motifs,
        method = method,
        ignore_linkages = FALSE
      )

      ctx <- ctx_add_data(ctx, "dynamic_motif_exp", dynamic_motif_exp)
      ctx <- ctx_add_table(
        ctx,
        "dynamic_motifs",
        tibble::as_tibble(dynamic_motif_exp),
        "Dynamic motif quantification results."
      )
      ctx
    },
    report = function(x) {
      tbl <- x$tables[["dynamic_motifs"]]
      n_motifs <- length(unique(tbl$motif))
      paste0(
        "Dynamic motif quantification was performed. ",
        "All motifs were extracted. ",
        "Number of quantified motifs: ",
        n_motifs,
        "."
      )
    },
    generate = "dynamic_motif_exp",
    require = "exp",
    signature = signature
  )
}

#' Step: Quantify Branch Motifs
#'
#' @description
#' Quantify N-glycan branch motifs using `glydet::quantify_motifs()` with `glymotif::branch_motifs()`.
#' This extracts specific N-glycan branching patterns (bi-antennary, tri-antennary, etc.).
#' Only works with N-glycans.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment to quantify motifs for (must be N-glycans)
#'
#' Data generated:
#' - `branch_motif_exp`: The experiment with quantified branch motifs
#'
#' Tables generated:
#' - `branch_motifs`: A table containing the quantified branch motifs.
#'
#' @section AI Prompt:
#' *This section is for AI in [inquire_blueprint()] only.*
#'
#' - Include this step if motif analysis is needed specifically for N-glycans.
#' - This step should be followed by DEA and visualization steps.
#'
#' @param method Method for motif quantification ("relative" or "absolute"). Default is "relative".
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_quantify_branch_motifs()
#' @seealso [glydet::quantify_motifs()], [glymotif::branch_motifs()]
#' @export
step_quantify_branch_motifs <- function(method = "relative") {
  signature <- rlang::expr_deparse(match.call())

  step(
    id = "quantify_branch_motifs",
    label = "Branch motif quantification",
    condition = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      if (!.has_glycan_structure(exp)) {
        return(list(
          check = FALSE,
          reason = "glycan structures are not available in the experiment"
        ))
      }
      type <- glyexp::get_glycan_type(exp)
      if (type != "N") {
        return(list(
          check = FALSE,
          reason = "branch motif quantification only works with N-glycans"
        ))
      }
      list(check = TRUE, reason = NULL)
    },
    run = function(ctx) {
      exp <- ctx_get_data(ctx, "exp")
      motifs <- glymotif::branch_motifs()

      branch_motif_exp <- glydet::quantify_motifs(
        exp,
        motifs = motifs,
        method = method,
        ignore_linkages = FALSE
      )

      ctx <- ctx_add_data(ctx, "branch_motif_exp", branch_motif_exp)
      ctx <- ctx_add_table(
        ctx,
        "branch_motifs",
        tibble::as_tibble(branch_motif_exp),
        "Branch motif quantification results."
      )
      ctx
    },
    report = function(x) {
      tbl <- x$tables[["branch_motifs"]]
      n_motifs <- length(unique(tbl$motif))
      paste0(
        "Branch motif quantification was performed. ",
        "Branching motifs for N-glycans were extracted. ",
        "Number of quantified motifs: ",
        n_motifs,
        "."
      )
    },
    generate = "branch_motif_exp",
    require = "exp",
    signature = signature
  )
}

#' Step: ROC Analysis
#'
#' @description
#' Perform ROC analysis using `glystats::gly_roc()`,
#' extract top 10 variables with highest AUC,
#' and plot ROC curves for these variables using `glyvis::plot_roc()`.
#'
#' This step requires `exp` (experiment data).
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
