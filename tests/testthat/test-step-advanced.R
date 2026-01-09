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

# ----- step_quantify_motifs -----
test_that("step_quantify_motifs generates motif_exp and tables", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_quantify_motifs())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("motif_exp" %in% names(res$data))
  expect_true("quantified_motifs" %in% names(res$tables))
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
