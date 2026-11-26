test_that("identity columns follow the result schema, not metadata names", {
  modern <- list(
    identifiers = list(columns = c(variable = ".variable", sample = ".sample"))
  )
  expect_identical(.stats_id_column(modern), ".variable")
  expect_identical(.stats_id_column(modern, "sample"), ".sample")
  expect_identical(.stats_id_column(list()), "variable")
  expect_identical(
    .stats_id_column(list(identifiers = list(data = "old"))),
    "variable"
  )

  tbl <- tibble::tibble(
    variable = c("annotation", "annotation"),
    .variable = c("V1", "V2"),
    p_adj = c(0.01, 0.02)
  )
  expect_equal(.dea_count_sig_total(tbl, .stats_id_column(modern)), 2L)
  expect_equal(.dea_count_sig_total(tbl, .stats_id_column(list())), 1L)
})

test_that("Cox reports use stored identity schema and preserve old contexts", {
  tbl <- tibble::tibble(
    variable = "annotation",
    .variable = "V1",
    hr = 2,
    p_adj = 0.01
  )
  ctx <- list(
    tables = list(cox = tbl),
    data = list(cox_id_column = ".variable")
  )
  expect_match(
    .report_cox(ctx, "cox", "exp"),
    "Top associated variable: V1",
    fixed = TRUE
  )
  ctx$data <- list()
  expect_match(
    .report_cox(ctx, "cox", "exp"),
    "Top associated variable: annotation",
    fixed = TRUE
  )
})
