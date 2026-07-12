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
  expect_true("uninferred_exp" %in% names(res$data))
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

  expect_true("inferred_structures" %in% names(res$tables))
  expect_equal(nrow(res$tables$inferred_structures), 3)
  expect_equal(res$tables$inferred_structures$variable, c("V1", "V2", "V3"))
  expect_equal(res$tables$inferred_structures$matched, c(TRUE, TRUE, FALSE))
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
  expect_true("trait_exp" %in% names(res$data))
  expect_true("derived_traits" %in% names(res$tables))
})

# ----- step_quantify_dynamic_motifs -----
test_that("step_quantify_dynamic_motifs generates dynamic_motif_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_quantify_dynamic_motifs())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("dynamic_motif_exp" %in% names(res$data))
  expect_true("dynamic_motifs" %in% names(res$tables))
})

# ----- step_quantify_branch_motifs -----
test_that("step_quantify_branch_motifs generates branch_motif_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_quantify_branch_motifs())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("branch_motif_exp" %in% names(res$data))
  expect_true("branch_motifs" %in% names(res$tables))
})

test_that("step_quantify_branch_motifs skips for non-N-glycans", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  # Modify the experiment to have O-glycan type
  # Must set after auto_clean() since it resets meta_data
  exp$meta_data$glycan_type <- "O"
  bp <- blueprint(step_quantify_branch_motifs())
  # Step should be skipped (message about skipping)
  expect_message(
    forge_analysis_se(exp, bp),
    "Skipping.*branch motif quantification only works with N-glycans"
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

  expect_true("roc_auc" %in% names(res$tables))
  expect_true("roc_curves" %in% names(res$plots))
})
