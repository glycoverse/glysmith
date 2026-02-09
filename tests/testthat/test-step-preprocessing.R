# ----- step_preprocess -----
test_that("step_preprocess overwrites exp and writes raw_exp", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  # We use sum to check if the expression matrix is changed
  old_sum <- sum(exp$expr_mat, na.rm = TRUE)
  bp <- blueprint(step_preprocess())
  suppressMessages(res <- forge_analysis(exp, bp))
  new_exp <- res$data$exp
  raw_exp <- res$data$raw_exp
  expect_equal(sum(raw_exp$expr_mat, na.rm = TRUE), old_sum)
  expect_false(sum(new_exp$expr_mat, na.rm = TRUE) == old_sum)
})

test_that("step_plot_qc generates plots with correct prefixes", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)

  # Test pre-QC with qc_pre_ prefix (includes missing value plots)
  bp_pre <- blueprint(step_plot_qc(when = "pre"))
  suppressMessages(res_pre <- forge_analysis(exp, bp_pre))
  expect_true("qc_pre_missing_heatmap" %in% names(res_pre$plots))
  expect_false("qc_missing_heatmap" %in% names(res_pre$plots))
  expect_true("qc_pre_tic_bar" %in% names(res_pre$plots)) # common plot with pre_ prefix

  # Test post-QC with standard qc_ prefix (missing value plots are pre-only)
  bp_post <- blueprint(step_plot_qc(when = "post"))
  suppressMessages(res_post <- forge_analysis(exp, bp_post))
  expect_true("qc_tic_bar" %in% names(res_post$plots)) # common plot uses standard prefix
  expect_false("qc_pre_tic_bar" %in% names(res_post$plots))
})

test_that("step_plot_qc can appear twice in a blueprint", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)

  bp <- blueprint(
    step_plot_qc(when = "pre"),
    step_preprocess(),
    step_plot_qc(when = "post")
  )
  expect_equal(length(bp), 3)
  expect_equal(names(bp)[1], "plot_qc_pre")
  expect_equal(names(bp)[3], "plot_qc_post")

  suppressMessages(res <- forge_analysis(exp, bp))
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
  bp <- blueprint(step_subset_groups(groups = c("H", "C")))
  suppressMessages(res <- forge_analysis(exp, bp))

  groups <- unique(as.character(res$data$exp$sample_info$group))
  expect_setequal(groups, c("H", "C"))
  expect_equal(levels(res$data$exp$sample_info$group), c("H", "C"))
  expect_true("full_exp" %in% names(res$data))
  expect_gt(
    length(unique(as.character(res$data$full_exp$sample_info$group))),
    length(groups)
  )
})

# ----- step_ident_overview -----
test_that("step_ident_overview generates summary", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(10)
  bp <- blueprint(step_ident_overview())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("summary" %in% names(res$tables))
})

# ----- step_adjust_protein -----
make_protein_expr <- function(exp) {
  protein_tbl <- exp$var_info |>
    dplyr::select(dplyr::all_of(c("variable", "protein")))

  expr_tbl <- tibble::as_tibble(exp$expr_mat, rownames = "variable") |>
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

  pro_expr <- make_protein_expr(exp)
  csv_path <- tempfile(fileext = ".csv")
  tsv_path <- tempfile(fileext = ".tsv")
  rds_path <- tempfile(fileext = ".rds")

  readr::write_csv(pro_expr$tbl, csv_path)
  readr::write_tsv(pro_expr$tbl, tsv_path)
  readr::write_rds(pro_expr$mat, rds_path)

  run_adjust <- function(path) {
    bp <- blueprint(step_adjust_protein(path))
    suppressMessages(forge_analysis(exp, bp))
  }

  purrr::walk(c(csv_path, tsv_path, rds_path), function(path) {
    res <- run_adjust(path)
    expect_true("unadj_exp" %in% names(res$data))
    expect_true("exp" %in% names(res$data))
    expect_false(
      isTRUE(all.equal(
        sum(res$data$exp$expr_mat, na.rm = TRUE),
        sum(res$data$unadj_exp$expr_mat, na.rm = TRUE)
      ))
    )
  })
})
