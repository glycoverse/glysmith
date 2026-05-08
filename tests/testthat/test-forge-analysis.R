local_mock_forge_pipeline <- function() {
  testthat::local_mocked_bindings(
    check_glysmith_deps = function(...) invisible(TRUE),
    run_blueprint = function(blueprint, ctx, quiet = FALSE) {
      ctx$meta$steps <- c(ctx$meta$steps, "mock_pipeline")
      ctx
    },
    .package = "glysmith",
    .env = rlang::caller_env()
  )
}

test_that("forge_analysis works for glycomics data", {
  local_mock_forge_pipeline()

  exp <- glyexp::real_experiment2
  suppressMessages(result <- forge_analysis(exp))
  expect_s3_class(result, "glysmith_result")
  expect_s3_class(result$exp, "glyexp_experiment")
  expect_true(is.factor(result$exp$sample_info$group))
  expect_equal(result$meta$steps, "mock_pipeline")
})

test_that("forge_analysis works for glycoproteomics data", {
  local_mock_forge_pipeline()

  exp <- glyexp::real_experiment
  suppressMessages(result <- forge_analysis(exp))
  expect_s3_class(result, "glysmith_result")
  expect_s3_class(result$exp, "glyexp_experiment")
  expect_true(is.factor(result$exp$sample_info$group))
  expect_equal(result$meta$steps, "mock_pipeline")
})
