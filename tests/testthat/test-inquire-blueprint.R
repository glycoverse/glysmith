
test_that("inquire_blueprint works with valid AI output", {
  skip_on_ci()

  local_mocked_bindings(.ask_ai = function(...) "step_preprocess(); step_pca()")
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  bp <- inquire_blueprint("dummy description")

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 2)
  expect_equal(bp[[1]]$id, "preprocess")
  expect_equal(bp[[2]]$id, "pca")
})

test_that("inquire_blueprint handles single step", {
  skip_on_ci()
  local_mocked_bindings(.ask_ai = function(...) "step_tsne(on = 'exp')")
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  bp <- inquire_blueprint("dummy description")

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 1)
  expect_equal(bp[[1]]$id, "tsne")
})

test_that("inquire_blueprint cleans AI output (backticks)", {
  skip_on_ci()
  local_mocked_bindings(.ask_ai = function(...) "`step_preprocess()`")
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  bp <- inquire_blueprint("dummy description")
  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 1)
  expect_equal(bp[[1]]$id, "preprocess")
})

test_that("inquire_blueprint raises error on invalid format", {
  skip_on_ci()
  local_mocked_bindings(.ask_ai = function(...) "I am not a blueprint")
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(inquire_blueprint("dummy description"), "Invalid AI output")
})

test_that("inquire_blueprint raises error on empty output", {
  skip_on_ci()
  local_mocked_bindings(.ask_ai = function(...) "")
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(inquire_blueprint("dummy description"), "Invalid AI output")
})

test_that("inquire_blueprint raises error on valid format but execution error", {
  skip_on_ci()
  # step_nonexistent doesn't exist, so eval() should fail
  local_mocked_bindings(.ask_ai = function(...) "step_nonexistent()")
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(inquire_blueprint("dummy description"), "Failed to parse AI output")
})

test_that("inquire_blueprint requires API key", {
  # No API key set
  withr::local_envvar(c(DEEPSEEK_API_KEY = ""))
  expect_error(inquire_blueprint("dummy"), "API key for DeepSeek chat model is not set")
})
