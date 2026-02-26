# ----- step_derive_traits -----
test_that("step_derive_traits generates trait_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_derive_traits())
  suppressMessages(res <- forge_analysis(exp, bp))
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
  suppressMessages(res <- forge_analysis(exp, bp))
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
  suppressMessages(res <- forge_analysis(exp, bp))
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
    forge_analysis(exp, bp),
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
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("roc_auc" %in% names(res$tables))
  expect_true("roc_curves" %in% names(res$plots))
})
