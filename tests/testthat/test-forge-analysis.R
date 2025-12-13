test_that("forge_analysis works", {
  # This is an integration test that checks if all parts of the system work together.
  exp <- glyexp::real_experiment2
  suppressMessages(result <- forge_analysis(exp))
  expect_s3_class(result, "glysmith_result")
})