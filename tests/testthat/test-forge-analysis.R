test_that("forge_analysis works", {
  # This is an integration test that checks if all parts of the system work together.
  exp <- glyexp::real_experiment2
  expect_snapshot(
    result <- forge_analysis(exp),
    transform = function(x) stringr::str_replace(x, "\\[.*\\]", "[<TIME>]")
  )
  expect_s3_class(result, "glysmith_result")
})