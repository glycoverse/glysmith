# ----- step_infer_structure -----
make_structure_inference_exp <- function() {
  expr_mat <- matrix(
    seq_len(12),
    nrow = 3,
    dimnames = list(
      c("V1", "V2", "V3"),
      c("S1", "S2", "S3", "S4")
    )
  )
  sample_info <- tibble::tibble(
    sample = c("S1", "S2", "S3", "S4"),
    group = factor(c("A", "A", "B", "B"))
  )
  var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan_composition = glyrepr::as_glycan_composition(c(
      "Man(2)GlcNAc(2)Fuc(1)",
      "Man(2)GlcNAc(3)",
      "Man(9)"
    ))
  )
  glyexp::experiment(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "glycomics",
    glycan_type = "N"
  )
}

make_structure_inference_db <- function() {
  db <- glyrepr::as_glycan_structure(c(
    "Man(??-?)Man(??-?)GlcNAc(??-?)[Fuc(??-?)]GlcNAc(??-",
    "GlcNAc(??-?)Man(??-?)Man(??-?)GlcNAc(??-?)GlcNAc(??-"
  ))
  attr(db, "confidence") <- c(2, 1)
  db
}

test_that("step_infer_structure adds structures and filters unmatched variables", {
  exp <- make_structure_inference_exp()
  db <- make_structure_inference_db()
  testthat::local_mocked_bindings(
    glydb_structures = function(structure_level, species, glycan_type) db,
    .package = "glydb"
  )
  bp <- blueprint(step_infer_structure())

  suppressMessages(res <- forge_analysis_se(exp, bp))

  expect_false("db" %in% names(formals(step_infer_structure)))
  expect_true("infer_structure" %in% names(glysmith:::all_steps()))
  var_info <- SummarizedExperiment::rowData(res$exp)
  expect_true("glycan_structure" %in% colnames(var_info))
  expect_false(any(is.na(var_info$glycan_structure)))
  expect_equal(rownames(var_info), c("V1", "V2"))
  expect_equal(rownames(res$exp), c("V1", "V2"))
  expect_true("node_1__uninferred_exp" %in% names(res$data))
})

test_that("step_infer_structure records inference results before filtering", {
  exp <- make_structure_inference_exp()
  db <- make_structure_inference_db()
  testthat::local_mocked_bindings(
    glydb_structures = function(structure_level, species, glycan_type) db,
    .package = "glydb"
  )
  bp <- blueprint(step_infer_structure())

  suppressMessages(res <- forge_analysis_se(exp, bp))

  expect_true("node_1__inferred_structures" %in% names(res$tables))
  expect_equal(nrow(res$tables$node_1__inferred_structures), 3)
  expect_equal(
    res$tables$node_1__inferred_structures$.variable,
    c("V1", "V2", "V3")
  )
  expect_equal(
    res$tables$node_1__inferred_structures$matched,
    c(TRUE, TRUE, FALSE)
  )
})

test_that("step_infer_structure builds database from species, structure level, and glycan type", {
  exp <- make_structure_inference_exp()
  db <- make_structure_inference_db()
  calls <- list()

  testthat::local_mocked_bindings(
    glydb_structures = function(structure_level, species, glycan_type) {
      calls[[length(calls) + 1L]] <<- list(
        structure_level = structure_level,
        species = species,
        glycan_type = glycan_type
      )
      db
    },
    .package = "glydb"
  )

  bp <- blueprint(step_infer_structure(species = "Homo sapiens"))
  suppressMessages(forge_analysis_se(exp, bp))

  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$structure_level, "topological")
  expect_equal(calls[[1]]$species, "Homo sapiens")
  expect_equal(calls[[1]]$glycan_type, "N")
})

test_that("step_infer_structure supports basic generic structure databases", {
  exp <- make_structure_inference_exp()
  db <- glyrepr::as_glycan_structure(c(
    "Hex(??-?)Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-",
    "HexNAc(??-?)Hex(??-?)Hex(??-?)HexNAc(??-?)HexNAc(??-"
  ))
  attr(db, "confidence") <- c(2, 1)
  testthat::local_mocked_bindings(
    glydb_structures = function(structure_level, species, glycan_type) db,
    .package = "glydb"
  )

  bp <- blueprint(step_infer_structure(structure_level = "basic"))
  suppressMessages(res <- forge_analysis_se(exp, bp))

  var_info <- SummarizedExperiment::rowData(res$exp)
  expect_equal(rownames(var_info), c("V1", "V2"))
  expect_false(any(is.na(var_info$glycan_structure)))
})

# ----- step_derive_traits -----
test_that("step_derive_traits generates trait_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_derive_traits())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1" %in% names(res$data))
  expect_true("node_1__derived_traits" %in% names(res$tables))
})

# ----- step_quantify_motifs -----
test_that("step_quantify_motifs quantifies custom named motifs and reports count", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_row(10) |>
      glyclean::auto_clean()
  )
  motifs <- c(
    HexNAc = "HexNAc(??-",
    dHex = "dHex(??-"
  )
  bp <- blueprint(
    step_quantify_motifs(
      motifs = motifs,
      method = "absolute",
      alignments = c("terminal", "substructure"),
      ignore_linkages = TRUE
    )
  )
  expect_true("quantify_motifs" %in% names(glysmith:::all_steps()))

  suppressMessages(res <- forge_analysis_se(exp, bp))

  expected <- glydet::quantify_motifs(
    as_test_glyco_se(exp),
    motifs = motifs,
    method = "absolute",
    alignments = c("terminal", "substructure"),
    ignore_linkages = TRUE
  )
  actual <- res$data$node_1
  expect_equal(
    SummarizedExperiment::assay(actual),
    SummarizedExperiment::assay(expected)
  )
  expect_equal(
    SummarizedExperiment::colData(actual),
    SummarizedExperiment::colData(expected)
  )
  expect_equal(
    SummarizedExperiment::rowData(actual)$trait,
    SummarizedExperiment::rowData(expected)$trait
  )
  expect_equal(
    as.character(SummarizedExperiment::rowData(actual)$motif_structure),
    as.character(SummarizedExperiment::rowData(expected)$motif_structure)
  )
  expect_true("node_1__motifs" %in% names(res$tables))
  expect_setequal(unique(res$tables$node_1__motifs$trait), names(motifs))
  expect_match(
    res$meta$reports$node_1,
    "Number of quantified motifs: 2\\."
  )
})

test_that("step_quantify_motifs supports glyrepr_structure motifs and downstream DEA", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_row(10) |>
      glyclean::auto_clean()
  )
  motifs <- glyrepr::as_glycan_structure(c("HexNAc(??-", "Hex(??-?)HexNAc(??-"))
  bp <- blueprint(step_quantify_motifs(motifs = motifs) %=>% step_dea_limma())

  suppressMessages(res <- forge_analysis_se(exp, bp))

  expect_identical(res$meta$nodes$node_1$status, "success")
  expect_identical(res$meta$nodes$node_2$status, "success")
  expect_true("node_1__motifs" %in% names(res$tables))
  expect_true("node_2__dea" %in% names(res$tables))
})

test_that("step_quantify_motifs skips when motifs are NULL or structures are absent", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_row(10) |>
      glyclean::auto_clean()
  )
  expect_message(
    res_null <- forge_analysis_se(exp, blueprint(step_quantify_motifs())),
    "skipped.*motifs are not provided"
  )
  expect_identical(res_null$meta$nodes$node_1$status, "skipped")

  exp_no_struc <- as_test_glyco_se(exp)
  SummarizedExperiment::rowData(exp_no_struc)$glycan_structure <- NULL
  expect_message(
    res_no_struc <- forge_analysis_se(
      exp_no_struc,
      blueprint(step_quantify_motifs(motifs = "HexNAc(??-"))
    ),
    "skipped.*glycan structures are not available in the experiment"
  )
  expect_identical(res_no_struc$meta$nodes$node_1$status, "skipped")
})

test_that("step_quantify_motifs validates inputs", {
  expect_error(step_quantify_motifs(motifs = character(0)))
  expect_error(step_quantify_motifs(motifs = ""))
  expect_error(step_quantify_motifs(motifs = NA_character_))
  expect_error(step_quantify_motifs(motifs = 123))
  expect_error(step_quantify_motifs(motifs = "HexNAc(??-", method = "invalid"))
  expect_error(step_quantify_motifs(
    motifs = "HexNAc(??-",
    alignments = "invalid"
  ))
  expect_error(step_quantify_motifs(
    motifs = "HexNAc(??-",
    ignore_linkages = "yes"
  ))
})

test_that("AI blueprint parsing handles step_quantify_motifs with custom motifs", {
  res <- glysmith:::.process_blueprint_response(
    '{"explanation":"Quantify custom motifs then run DEA.","steps":["step_quantify_motifs(motifs = c(Lx = \\"Hex(??-?)[dHex(??-?)]HexNAc(??-\\")) %=>% step_dea_limma()"]}'
  )
  expect_true(res$valid)
  nodes <- .compile_blueprint(res$blueprint)$nodes
  expect_identical(
    unname(vapply(nodes, function(n) n$step$id, "")),
    c("quantify_motifs", "dea_limma")
  )
})

# ----- step_quantify_dynamic_motifs -----
test_that("step_quantify_dynamic_motifs generates dynamic_motif_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_row(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_quantify_dynamic_motifs())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1" %in% names(res$data))
  expect_true("node_1__dynamic_motifs" %in% names(res$tables))
  expect_match(
    res$meta$reports$node_1,
    "Dynamic motif quantification was performed\\. All motifs were extracted\\. Number of quantified motifs:"
  )
})

test_that("step_quantify_dynamic_motifs validates inputs", {
  expect_error(step_quantify_dynamic_motifs(max_size = 0))
  expect_error(step_quantify_dynamic_motifs(method = "invalid"))
})

# ----- step_quantify_branch_motifs -----
test_that("step_quantify_branch_motifs generates branch_motif_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_row(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_quantify_branch_motifs())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("node_1" %in% names(res$data))
  expect_true("node_1__branch_motifs" %in% names(res$tables))
  expect_match(
    res$meta$reports$node_1,
    "Branch motif quantification was performed\\. Branching motifs for N-glycans were extracted\\. Number of quantified motifs:"
  )
})

test_that("step_quantify_branch_motifs validates inputs", {
  expect_error(step_quantify_branch_motifs(method = "invalid"))
})

test_that("step_quantify_branch_motifs skips for non-N-glycans", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  # Modify the experiment to have O-glycan type
  # Must set after auto_clean() since it resets meta_data
  exp <- as_test_glyco_se(exp)
  S4Vectors::metadata(exp)$glycan_type <- "O"
  bp <- blueprint(step_quantify_branch_motifs())
  # Step should be skipped (message about skipping)
  expect_message(
    forge_analysis_se(exp, bp),
    "skipped.*branch motif quantification only works with N-glycans"
  )
})

# ----- step_roc -----
test_that("step_roc generates results", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_roc())
  suppressMessages(res <- forge_analysis_se(exp, bp))

  expect_true("node_1__roc_auc" %in% names(res$tables))
  expect_true("node_1__roc_curves" %in% names(res$plots))
})
