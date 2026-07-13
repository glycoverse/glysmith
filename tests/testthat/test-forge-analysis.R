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

test_that("forge_analysis works natively for GlycomicSE data", {
  local_mock_forge_pipeline()

  exp <- glyexp::as_glycomic_se(glyexp::real_experiment2)
  suppressMessages(result <- forge_analysis_se(exp))
  expect_s3_class(result, "glysmith_result")
  expect_s4_class(result$exp, "GlycomicSE")
  expect_true(is.factor(SummarizedExperiment::colData(result$exp)$group))
  expect_equal(result$meta$steps, "mock_pipeline")
})

test_that("forge_analysis works natively for GlycoproteomicSE data", {
  local_mock_forge_pipeline()

  exp <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)
  suppressMessages(result <- forge_analysis_se(exp))
  expect_s3_class(result, "glysmith_result")
  expect_s4_class(result$exp, "GlycoproteomicSE")
  expect_true(is.factor(SummarizedExperiment::colData(result$exp)$group))
  expect_equal(result$meta$steps, "mock_pipeline")
})

test_that("forge_analysis keeps experiment backward compatibility", {
  local_mock_forge_pipeline()

  exp <- glyexp::real_experiment2
  suppressMessages(result <- glysmith::forge_analysis(exp))
  expect_s3_class(result$exp, "glyexp_experiment")
  expect_true(is.factor(result$exp$sample_info$group))
})
