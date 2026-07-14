# ----- step_preprocess -----
test_that("step_preprocess overwrites exp and writes raw_exp", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  exp <- as_test_glyco_se(exp)
  # We use sum to check if the expression matrix is changed
  old_sum <- sum(SummarizedExperiment::assay(exp), na.rm = TRUE)
  bp <- blueprint(step_preprocess())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  new_exp <- res$data$exp
  raw_exp <- res$data$raw_exp
  expect_equal(
    sum(SummarizedExperiment::assay(raw_exp), na.rm = TRUE),
    old_sum
  )
  expect_false(
    sum(SummarizedExperiment::assay(new_exp), na.rm = TRUE) == old_sum
  )
})

test_that("step_preprocess keeps QC samples if present", {
  # Create experiment with QC samples
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  exp <- as_test_glyco_se(exp)
  # Extend sample_info with QC samples
  new_sample_info <- dplyr::bind_rows(
    tibble::as_tibble(SummarizedExperiment::colData(exp), rownames = "sample"),
    tibble::tibble(
      sample = c("QC1", "QC2"),
      group = factor(
        "QC",
        levels = c(
          levels(SummarizedExperiment::colData(exp)$group),
          "QC"
        )
      )
    )
  )

  # Extend expr_mat with QC columns (duplicate existing samples as QC)
  new_expr_mat <- cbind(
    SummarizedExperiment::assay(exp),
    QC1 = SummarizedExperiment::assay(exp)[, "C1"],
    QC2 = SummarizedExperiment::assay(exp)[, "C2"]
  )

  # Create new experiment with QC samples
  exp_with_qc <- glyexp::GlycoproteomicSE(
    abundance = new_expr_mat,
    colData = S4Vectors::DataFrame(
      dplyr::select(new_sample_info, -"sample"),
      row.names = new_sample_info$sample
    ),
    rowData = SummarizedExperiment::rowData(exp),
    metadata = S4Vectors::metadata(exp)
  )

  # Verify QC samples exist before preprocessing
  groups_before <- unique(as.character(
    SummarizedExperiment::colData(exp_with_qc)$group
  ))
  expect_true("QC" %in% groups_before)
  expect_equal(ncol(exp_with_qc), 14) # 12 original + 2 QC

  # Run preprocessing
  bp <- blueprint(step_preprocess())
  suppressMessages(res <- forge_analysis_se(exp_with_qc, bp))

  # Verify QC samples are left for users to handle explicitly.
  sample_info_after <- tibble::as_tibble(
    SummarizedExperiment::colData(res$exp),
    rownames = "sample"
  )
  groups_after <- unique(as.character(sample_info_after$group))
  samples_after <- sample_info_after$sample
  expect_true("QC" %in% groups_after)
  expect_true(all(c("QC1", "QC2") %in% samples_after))
})

test_that("step_preprocess does not pass qc_name to auto_clean", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  exp <- as_test_glyco_se(exp)
  passed_args <- NULL

  local_mocked_bindings(
    auto_clean = function(exp, ...) {
      passed_args <<- names(list(...))
      exp
    },
    .package = "glyclean"
  )

  ctx <- glysmith:::new_ctx(exp, "group")
  suppressMessages(glysmith:::.run_preprocess(
    ctx,
    batch_col = "batch",
    normalize_to_try = NULL,
    impute_to_try = NULL,
    remove_preset = "discovery",
    batch_prop_threshold = 0.3,
    check_batch_confounding = TRUE,
    batch_confounding_threshold = 0.4
  ))

  expect_false("qc_name" %in% passed_args)
})

test_that("step_plot_qc generates plots with correct prefixes", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  exp <- as_test_glyco_se(exp)

  # Test pre-QC with qc_pre_ prefix (includes missing value plots)
  bp_pre <- blueprint(step_plot_qc(when = "pre"))
  suppressMessages(res_pre <- forge_analysis_se(exp, bp_pre))
  expect_true("qc_pre_missing_heatmap" %in% names(res_pre$plots))
  expect_false("qc_missing_heatmap" %in% names(res_pre$plots))
  expect_true("qc_pre_tic_bar" %in% names(res_pre$plots)) # common plot with pre_ prefix

  # Test post-QC with standard qc_ prefix (missing value plots are pre-only)
  bp_post <- blueprint(step_plot_qc(when = "post"))
  suppressMessages(res_post <- forge_analysis_se(exp, bp_post))
  expect_true("qc_tic_bar" %in% names(res_post$plots)) # common plot uses standard prefix
  expect_false("qc_pre_tic_bar" %in% names(res_post$plots))
})

test_that("step_plot_qc can appear twice in a blueprint", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  exp <- as_test_glyco_se(exp)

  bp <- blueprint(
    step_plot_qc(when = "pre"),
    step_preprocess(),
    step_plot_qc(when = "post")
  )
  expect_equal(length(bp), 3)
  expect_equal(names(bp)[1], "plot_qc_pre")
  expect_equal(names(bp)[3], "plot_qc_post")

  suppressMessages(res <- forge_analysis_se(exp, bp))
  # Pre-QC has pre_ prefix for all plots including common ones
  expect_true("qc_pre_missing_heatmap" %in% names(res$plots))
  expect_true("qc_pre_tic_bar" %in% names(res$plots))
  # Post-QC has standard prefix for common plots (missing plots are pre-only)
  expect_true("qc_tic_bar" %in% names(res$plots))
  # Both pre and post QC plots exist
  expect_true("qc_pre_tic_bar" %in% names(res$plots))
})

# ----- step_subset_groups -----
test_that("step_subset_groups filters exp and keeps full_exp", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  exp <- as_test_glyco_se(exp)
  bp <- blueprint(step_subset_groups(groups = c("H", "C")))
  suppressMessages(res <- forge_analysis_se(exp, bp))

  sample_info <- tibble::as_tibble(
    SummarizedExperiment::colData(res$data$exp),
    rownames = "sample"
  )
  groups <- unique(as.character(sample_info$group))
  expect_setequal(groups, c("H", "C"))
  expect_equal(levels(sample_info$group), c("H", "C"))
  expect_true("full_exp" %in% names(res$data))
  full_sample_info <- SummarizedExperiment::colData(res$data$full_exp)
  expect_gt(
    length(unique(as.character(full_sample_info$group))),
    length(groups)
  )
})

# ----- step_ident_overview -----
test_that("step_ident_overview generates summary", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  exp <- as_test_glyco_se(exp)
  bp <- blueprint(step_ident_overview())
  suppressMessages(res <- forge_analysis_se(exp, bp))
  expect_true("summary" %in% names(res$tables))
})

# ----- step_adjust_protein -----
make_protein_expr <- function(exp) {
  protein_tbl <- tibble::as_tibble(
    SummarizedExperiment::rowData(exp),
    rownames = "variable"
  ) |>
    dplyr::select(dplyr::all_of(c("variable", "protein")))

  expr_tbl <- tibble::as_tibble(
    SummarizedExperiment::assay(exp),
    rownames = "variable"
  ) |>
    dplyr::left_join(protein_tbl, by = "variable") |>
    dplyr::group_by(.data$protein) |>
    dplyr::summarise(
      dplyr::across(
        -dplyr::all_of("variable"),
        ~ stats::median(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  list(
    tbl = expr_tbl,
    mat = expr_tbl |>
      tibble::column_to_rownames("protein") |>
      as.matrix()
  )
}

test_that("step_adjust_protein adjusts exp from csv/tsv/rds", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(50)
  exp <- as_test_glyco_se(exp)

  pro_expr <- make_protein_expr(exp)
  csv_path <- tempfile(fileext = ".csv")
  tsv_path <- tempfile(fileext = ".tsv")
  rds_path <- tempfile(fileext = ".rds")

  readr::write_csv(pro_expr$tbl, csv_path)
  readr::write_tsv(pro_expr$tbl, tsv_path)
  readr::write_rds(pro_expr$mat, rds_path)

  run_adjust <- function(path) {
    bp <- blueprint(step_adjust_protein(path))
    suppressMessages(forge_analysis_se(exp, bp))
  }

  purrr::walk(c(csv_path, tsv_path, rds_path), function(path) {
    res <- run_adjust(path)
    expect_true("unadj_exp" %in% names(res$data))
    expect_true("exp" %in% names(res$data))
    expect_false(
      isTRUE(all.equal(
        sum(SummarizedExperiment::assay(res$data$exp), na.rm = TRUE),
        sum(SummarizedExperiment::assay(res$data$unadj_exp), na.rm = TRUE)
      ))
    )
  })
})
