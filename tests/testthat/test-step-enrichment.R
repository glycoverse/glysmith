# ----- step_sig_enrich -----
run_sig_enrich_step <- function(step_fun, kind) {
  skip_if_not_installed("glyfun")
  skip_if_not_installed("enrichplot")

  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::filter_obs(group %in% c("H", "C")) |>
      glyexp::mutate_obs(group = factor(group)) |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )

  ctx <- new_ctx(exp, "group")
  dea_res <- structure(
    list(tidy_result = tibble::tibble(), meta_data = list()),
    class = c("glystats_ttest_res", "glystats_res")
  )
  ctx <- ctx_add_data(ctx, "dea_res", dea_res)

  mock_tbl <- tibble::tibble(
    description = c("term_a", "term_b"),
    p_adj = c(0.01, 0.2)
  )
  called <- new.env(parent = emptyenv())
  mock_enrich <- function(dea_res, ...) {
    called$dea_res <- dea_res
    called$args <- list(...)
    structure(mock_tbl, class = c("mock_enrich", class(mock_tbl)))
  }
  local_mocked_bindings(
    enrich_ora_go = mock_enrich,
    enrich_ora_kegg = mock_enrich,
    enrich_ora_reactome = mock_enrich,
    enrich_ora_ncg = mock_enrich,
    enrich_ora_wp = mock_enrich,
    enrich_ora_do = mock_enrich,
    .package = "glyfun"
  )
  local_mocked_bindings(
    dotplot = function(...) ggplot2::ggplot(),
    .package = "enrichplot"
  )

  step_obj <- step_fun()
  expect_equal(step_obj$require, c("exp", "dea_res"))
  expect_true(step_obj$condition(ctx)$check)
  ctx <- step_obj$run(ctx)
  expect_identical(called$dea_res, dea_res)
  expect_true(kind %in% names(ctx$tables))
  expect_true(kind %in% names(ctx$plots))
  invisible(called)
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

test_that("step_sig_enrich_ncg generates results", {
  run_sig_enrich_step(step_sig_enrich_ncg, "ncg")
})

test_that("step_sig_enrich_wp generates results", {
  run_sig_enrich_step(step_sig_enrich_wp, "wp")
})

test_that("step_sig_enrich_do generates results", {
  run_sig_enrich_step(step_sig_enrich_do, "do")
})

test_that("step_sig_enrich passes detected proteins as universe", {
  called <- run_sig_enrich_step(
    function() step_sig_enrich_go(universe = "detected"),
    "go"
  )

  expect_type(called$args$universe, "character")
  expect_false(inherits(called$args$universe, "glyexp_experiment"))
})
