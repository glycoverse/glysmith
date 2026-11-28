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
#' @param species Species name used to restrict the glycan structure database.
#'   Default is `NULL`, which does not restrict by species.
#' @param structure_level Structure level passed to [glydb::glydb_structures()].
#'   One of `"intact"`, `"topological"`, or `"basic"`.
#'   Default is `"topological"`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_infer_structure()
#' step_infer_structure(species = "Homo sapiens")
#' @seealso [glyanno::comp_to_struc()], [glydb::glydb_structures()]
#' @export
step_infer_structure <- function(
  species = NULL,
  structure_level = "topological"
) {
  checkmate::assert_string(species, null.ok = TRUE)
  checkmate::assert_choice(structure_level, c("intact", "topological", "basic"))

  signature <- rlang::expr_deparse(match.call())

  step(
    id = "infer_structure",
    connected = .exp_port("exp"),
    label = "Glycan structure inference",
    condition = .condition_infer_structure,
    run = function(ctx) {
      .run_infer_structure(
        ctx,
        structure_level = structure_level,
        species = species
      )
    },
    report = .report_infer_structure,
    packages = c("glyanno", "glydb", "glyrepr"),
    signature = signature
  )
}

#' Check whether glycan structure inference should run
#'
#' @param ctx Analysis context.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_infer_structure <- function(ctx) {
  exp <- ctx_get_data(ctx, "exp")
  if (.has_glycan_structure(exp)) {
    return(list(
      check = FALSE,
      reason = "glycan structures are already available in the experiment"
    ))
  }
  if (!"glycan_composition" %in% colnames(.get_var_info(exp))) {
    return(list(
      check = FALSE,
      reason = "glycan compositions are not available in the experiment"
    ))
  }
  list(check = TRUE, reason = NULL)
}

#' Run glycan structure inference
#'
#' @param ctx Analysis context.
#' @param structure_level Structure level passed to [glydb::glydb_structures()].
#' @param species Species name used to restrict the glycan structure database.
#'
#' @returns Updated analysis context.
#' @noRd
.run_infer_structure <- function(ctx, structure_level, species) {
  exp <- ctx_get_data(ctx, "exp")
  db_to_use <- glydb::glydb_structures(
    structure_level = structure_level,
    species = species,
    glycan_type = .get_glycan_type(exp)
  )

  var_info <- .get_var_info(exp)
  comps <- var_info[["glycan_composition"]]
  if (isTRUE(all(glyrepr::get_mono_type(db_to_use) == "generic"))) {
    comps <- glyrepr::convert_to_generic(comps)
  }

  inferred_structures <- glyanno::comp_to_struc(
    comps,
    db = db_to_use,
    return_best = TRUE
  )
  inference_info <- var_info
  names(inference_info) <- make.unique(c(".variable", names(inference_info)))[
    -1
  ]
  inference_tbl <- tibble::tibble(
    .variable = rownames(exp),
    !!!inference_info
  ) |>
    dplyr::mutate(
      glycan_structure = inferred_structures,
      matched = !is.na(.data$glycan_structure)
    )

  var_info[["glycan_structure"]] <- inferred_structures
  inferred_exp <- .set_var_info(exp, var_info)
  inferred_exp <- inferred_exp[!is.na(inferred_structures), ]

  ctx <- ctx_add_data(ctx, "exp", inferred_exp)
  ctx <- ctx_add_data(ctx, "uninferred_exp", exp)
  ctx <- ctx_add_table(
    ctx,
    "inferred_structures",
    inference_tbl,
    "Glycan structure inference results."
  )
  ctx
}

#' Report glycan structure inference results
#'
#' @param x Analysis context after running structure inference.
#'
#' @returns A summary string for the report.
#' @noRd
.report_infer_structure <- function(x) {
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
    connected = .exp_port("trait_exp"),
    label = "Derived trait calculation",
    condition = .condition_trait_motif_input,
    run = function(ctx) {
      .run_derive_traits(
        ctx,
        trait_fns = trait_fns,
        mp_fns = mp_fns,
        mp_cols = mp_cols
      )
    },
    report = function(x) {
      .report_derive_traits(x)
    },
    packages = "glydet",
    signature = signature
  )
}

#' Check the common input requirements for trait and motif quantification
#'
#' @param ctx Analysis context.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_trait_motif_input <- function(ctx) {
  exp <- ctx_get_data(ctx, "exp")
  if (identical(glyexp::get_scale(.as_glyco_se(exp)), "logx")) {
    return(list(
      check = FALSE,
      reason = "trait and motif quantification requires linear abundances. Run it before step_coda()"
    ))
  }
  check <- .has_glycan_structure(exp)
  reason <- "glycan structures are not available in the experiment"
  list(check = check, reason = reason)
}


#' Run derived trait calculation
#'
#' @param ctx Analysis context.
#' @inheritParams step_derive_traits
#'
#' @returns Updated analysis context.
#' @noRd
.run_derive_traits <- function(ctx, trait_fns, mp_fns, mp_cols) {
  exp <- ctx_get_data(ctx, "exp")
  trait_exp <- glydet::derive_traits(
    exp,
    trait_fns = trait_fns,
    mp_fns = mp_fns,
    mp_cols = mp_cols
  )
  ctx <- ctx_add_data(ctx, "trait_exp", trait_exp)
  ctx_add_table(
    ctx,
    "derived_traits",
    .as_data_tibble(trait_exp),
    "Derived trait calculation results."
  )
}

#' Report derived trait calculation results
#'
#' @param x Analysis context after running derived trait calculation.
#'
#' @returns A summary string for the report.
#' @noRd
.report_derive_traits <- function(x) {
  tbl <- x$tables[["derived_traits"]]
  if (.get_exp_type(x$exp) == "glycomics") {
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
  paste0(msg, "\n\n", "Trait definitions:\n\n", definition_parts)
}

#' Step: Quantify Motifs
#'
#' @description
#' Quantify a custom set of glycan motifs using `glydet::quantify_motifs()`.
#' Supports known motif names from the GlycoMotif database, glycan structure
#' strings (optionally named), `glyrepr_structure` vectors, or motif
#' specifications from `glymotif`. Works with any glycan type.
#'
#' This step requires `exp` (experiment data).
#'
#' @details
#' Data required:
#' - `exp`: The experiment to quantify motifs for
#'
#' Data generated:
#' - `motif_exp`: The experiment with quantified motifs
#'
#' Tables generated:
#' - `motifs`: A table containing the quantified motifs.
#'
#' @param motifs Custom motif set passed to [glydet::quantify_motifs()]. Can be
#'   a character vector of known motif names or glycan structure strings
#'   (optionally named), a `glyrepr_structure` object, or a motif specification
#'   from `glymotif::dynamic_motifs()`, `glymotif::branch_motifs()`, or
#'   `glymotif::db_motifs()`. If `NULL`, this step will be skipped.
#' @param method Method for motif quantification (`"relative"` or `"absolute"`).
#'   Default is `"relative"`.
#' @param alignments Optional character vector specifying the alignment method
#'   for each motif. Values can be `"terminal"`, `"substructure"`, `"core"`,
#'   `"whole"`, or `"exact"`. Default is `NULL`, which uses `"substructure"`
#'   (or the database motif alignments when applicable).
#' @param ignore_linkages Logical; if `TRUE`, glycosidic linkages are ignored
#'   when matching motifs. Default is `FALSE`.
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_quantify_motifs(
#'   motifs = c(
#'     Lx = "Hex(??-?)[dHex(??-?)]HexNAc(??-",
#'     sLx = "NeuAc(??-?)Hex(??-?)[dHex(??-?)]HexNAc(??-"
#'   )
#' )
#' step_quantify_motifs(motifs = c("Lewis x", "Sialyl Lewis x"))
#' @seealso [glydet::quantify_motifs()], [glymotif::have_motifs()]
#' @export
step_quantify_motifs <- function(
  motifs = NULL,
  method = "relative",
  alignments = NULL,
  ignore_linkages = FALSE
) {
  if (!is.null(motifs)) {
    if (is.character(motifs)) {
      checkmate::assert_character(
        motifs,
        min.chars = 1,
        any.missing = FALSE,
        min.len = 1
      )
    } else if (inherits(motifs, "glyrepr_structure")) {
      if (length(motifs) == 0L || any(is.na(motifs))) {
        cli::cli_abort(
          "{.arg motifs} must be a non-empty {.cls glyrepr_structure} vector without missing values."
        )
      }
    } else if (
      !inherits(
        motifs,
        c("dynamic_motifs_spec", "branch_motifs_spec", "db_motifs_spec")
      )
    ) {
      cli::cli_abort(
        "{.arg motifs} must be a character vector, a {.cls glyrepr_structure} object, a motif specification, or {.code NULL}."
      )
    }
  }
  signature <- rlang::expr_deparse(match.call())

  .step_quantify_motifs(
    id = "quantify_motifs",
    label = "Motif quantification",
    exp_key = "motif_exp",
    table_key = "motifs",
    table_desc = "Motif quantification results.",
    report_prefix = "Motif quantification was performed. ",
    motifs = motifs,
    method = method,
    alignments = alignments,
    ignore_linkages = ignore_linkages,
    condition = function(ctx) {
      .condition_quantify_motifs(ctx, motifs = motifs)
    },
    signature = signature
  )
}

#' Build a motif quantification step
#'
#' @param id Step id.
#' @param label Step label.
#' @param exp_key Context data key for the quantified experiment.
#' @param table_key Context table key for the quantified motif table.
#' @param table_desc Description for the quantified motif table.
#' @param report_prefix Prefix text for the step report.
#' @param motifs Motif specification, vector, or zero-argument factory function.
#' @param method Method for motif quantification (`"relative"` or `"absolute"`).
#' @param alignments Optional alignment method vector.
#' @param ignore_linkages Logical; whether to ignore linkages.
#' @param condition Condition function for the step.
#' @param signature Step call signature.
#'
#' @returns A `glysmith_step` object.
#' @noRd
.step_quantify_motifs <- function(
  id,
  label,
  exp_key,
  table_key,
  table_desc,
  report_prefix,
  motifs,
  method = "relative",
  alignments = NULL,
  ignore_linkages = FALSE,
  condition = .condition_trait_motif_input,
  signature = NULL
) {
  checkmate::assert_choice(method, c("relative", "absolute"))
  if (!is.null(alignments)) {
    checkmate::assert_character(alignments, any.missing = FALSE, min.len = 1)
    checkmate::assert_subset(
      alignments,
      c("terminal", "substructure", "core", "whole", "exact")
    )
  }
  checkmate::assert_flag(ignore_linkages)

  step(
    id = id,
    connected = .exp_port(exp_key),
    label = label,
    condition = condition,
    run = function(ctx) {
      .run_quantify_motifs(
        ctx,
        motifs = motifs,
        method = method,
        alignments = alignments,
        ignore_linkages = ignore_linkages,
        exp_key = exp_key,
        table_key = table_key,
        table_desc = table_desc
      )
    },
    report = function(x) {
      .report_quantify_motifs(
        x,
        table_key = table_key,
        prefix = report_prefix
      )
    },
    packages = c("glydet", "glymotif"),
    signature = signature
  )
}

#' Check whether custom motif quantification should run
#'
#' @param ctx Analysis context.
#' @inheritParams step_quantify_motifs
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_quantify_motifs <- function(ctx, motifs) {
  if (is.null(motifs)) {
    return(list(check = FALSE, reason = "motifs are not provided"))
  }
  .condition_trait_motif_input(ctx)
}

#' Run motif quantification
#'
#' @param ctx Analysis context.
#' @inheritParams .step_quantify_motifs
#'
#' @returns Updated analysis context.
#' @noRd
.run_quantify_motifs <- function(
  ctx,
  motifs,
  method,
  alignments,
  ignore_linkages,
  exp_key,
  table_key,
  table_desc
) {
  exp <- ctx_get_data(ctx, "exp")
  motifs <- if (is.function(motifs)) motifs() else motifs
  motif_exp <- glydet::quantify_motifs(
    exp,
    motifs = motifs,
    method = method,
    alignments = alignments,
    ignore_linkages = ignore_linkages
  )

  ctx <- ctx_add_data(ctx, exp_key, motif_exp)
  ctx_add_table(
    ctx,
    table_key,
    .as_data_tibble(motif_exp),
    table_desc
  )
}

#' Report motif quantification results
#'
#' @param x Analysis context after running motif quantification.
#' @param table_key Context table key for the quantified motif table.
#' @param prefix Prefix text for the report summary.
#'
#' @returns A summary string for the report.
#' @noRd
.report_quantify_motifs <- function(x, table_key, prefix) {
  tbl <- x$tables[[table_key]]
  n_motifs <- length(unique(tbl$trait))
  paste0(
    prefix,
    "Number of quantified motifs: ",
    n_motifs,
    "."
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
#' @param max_size Maximum size of motifs to extract. Default is 3.
#' @param method Method for motif quantification ("relative" or "absolute"). Default is "relative".
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_quantify_dynamic_motifs()
#' @seealso [glydet::quantify_motifs()], [glymotif::dynamic_motifs()]
#' @export
step_quantify_dynamic_motifs <- function(max_size = 3, method = "relative") {
  checkmate::assert_count(max_size, positive = TRUE)
  signature <- rlang::expr_deparse(match.call())

  .step_quantify_motifs(
    id = "quantify_dynamic_motifs",
    label = "Dynamic motif quantification",
    exp_key = "dynamic_motif_exp",
    table_key = "dynamic_motifs",
    table_desc = "Dynamic motif quantification results.",
    report_prefix = "Dynamic motif quantification was performed. All motifs were extracted. ",
    motifs = function() glymotif::dynamic_motifs(max_size = max_size),
    method = method,
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
#' @param method Method for motif quantification ("relative" or "absolute"). Default is "relative".
#'
#' @return A `glysmith_step` object.
#' @examples
#' step_quantify_branch_motifs()
#' @seealso [glydet::quantify_motifs()], [glymotif::branch_motifs()]
#' @export
step_quantify_branch_motifs <- function(method = "relative") {
  signature <- rlang::expr_deparse(match.call())

  .step_quantify_motifs(
    id = "quantify_branch_motifs",
    label = "Branch motif quantification",
    exp_key = "branch_motif_exp",
    table_key = "branch_motifs",
    table_desc = "Branch motif quantification results.",
    report_prefix = "Branch motif quantification was performed. Branching motifs for N-glycans were extracted. ",
    motifs = function() glymotif::branch_motifs(),
    method = method,
    condition = .condition_quantify_branch_motifs,
    signature = signature
  )
}

#' Check whether branch motif quantification should run
#'
#' @param ctx Analysis context.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_quantify_branch_motifs <- function(ctx) {
  input <- .condition_trait_motif_input(ctx)
  if (!input$check) {
    return(input)
  }
  type <- .get_glycan_type(ctx_get_data(ctx, "exp"))
  if (type != "N") {
    return(list(
      check = FALSE,
      reason = "branch motif quantification only works with N-glycans"
    ))
  }
  list(check = TRUE, reason = NULL)
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
    connected = .exp_port(),
    label = "ROC analysis",
    condition = function(ctx) {
      .condition_roc(ctx)
    },
    run = function(ctx) {
      .run_roc(
        ctx,
        pos_class = pos_class,
        plot_width = plot_width,
        plot_height = plot_height
      )
    },
    report = function(x) {
      .report_roc(x)
    },
    packages = c("glystats", "glyvis", "pROC"),
    signature = signature
  )
}

#' Check whether ROC analysis should run
#'
#' @param ctx Analysis context.
#'
#' @returns A list with `check` and `reason`.
#' @noRd
.condition_roc <- function(ctx) {
  groups <- .get_sample_info(ctx_get_data(ctx, "exp"))[["group"]]
  if (length(unique(groups)) == 2) {
    list(check = TRUE, reason = NULL)
  } else {
    list(check = FALSE, reason = "more than 2 groups are in the experiment")
  }
}

#' Run ROC analysis
#'
#' @param ctx Analysis context.
#' @inheritParams step_roc
#'
#' @returns Updated analysis context.
#' @noRd
.run_roc <- function(ctx, pos_class, plot_width, plot_height) {
  exp <- ctx_get_data(ctx, "exp")
  roc_res <- glystats::gly_roc(exp, pos_class = pos_class)

  ctx <- ctx_add_table(
    ctx,
    "roc_auc",
    roc_res$tidy_result$auc,
    "ROC AUC values for all variables."
  )

  top_vars <- roc_res$tidy_result$auc |>
    dplyr::slice_max(.data$auc, n = 10, with_ties = FALSE) |>
    dplyr::pull(.stats_id_column(roc_res))

  sub_exp <- .filter_variables(exp, top_vars)

  p_roc <- glyvis::plot_roc(sub_exp, type = "roc")

  ctx_add_plot(
    ctx,
    "roc_curves",
    p_roc,
    "ROC curves for top 10 variables.",
    width = plot_width,
    height = plot_height
  )
}

#' Report ROC analysis results
#'
#' @param x Analysis context after running ROC analysis.
#'
#' @returns A summary string for the report.
#' @noRd
.report_roc <- function(x) {
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
}
