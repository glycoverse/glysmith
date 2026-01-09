# ----- step_sig_enrich -----
run_sig_enrich_step <- function(step_fun, kind) {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("org.Hs.eg.db")

  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )

  ctx <- new_ctx(exp, "group")
  ctx <- ctx_add_data(ctx, "sig_exp", exp)

  mock_tbl <- tibble::tibble(
    description = c("term_a", "term_b"),
    p_adj = c(0.01, 0.2)
  )
  local_mocked_bindings(
    gly_enrich_go = function(...) structure(list(), class = "mock_enrich"),
    gly_enrich_kegg = function(...) structure(list(), class = "mock_enrich"),
    gly_enrich_reactome = function(...) structure(list(), class = "mock_enrich"),
    get_tidy_result = function(...) mock_tbl,
    .package = "glystats"
  )
  local_mocked_bindings(
    plot_enrich = function(...) ggplot2::ggplot(),
    .package = "glyvis"
  )

  step_obj <- step_fun()
  expect_true(step_obj$condition(ctx)$check)
  ctx <- step_obj$run(ctx)
  expect_true(kind %in% names(ctx$tables))
  expect_true(kind %in% names(ctx$plots))
}

test_that("step_sig_enrich_go generates results", {
  run_sig_enrich_step(step_sig_enrich_go, "go")
})

test_that("step_sig_enrich_kegg generates results", {
  run_sig_enrich_step(step_sig_enrich_kegg, "kegg")
})

test_that("step_sig_enrich_reactome generates results", {
  run_sig_enrich_step(step_sig_enrich_reactome, "reactome")
})
