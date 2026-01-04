test_that("inquire_blueprint works with valid AI output", {
  skip_on_ci()
  skip_if_not_installed("ellmer")

  # Mock `ellmer::chat_deepseek` to return a chat object with a mocked `chat` method
  mock_chat_fun <- function(...) "step_preprocess(); step_pca()"
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  bp <- inquire_blueprint("dummy description")

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 2)
  expect_equal(bp[[1]]$id, "preprocess")
  expect_equal(bp[[2]]$id, "pca")
})

test_that("inquire_blueprint supports branches", {
  skip_on_ci()
  skip_if_not_installed("ellmer")

  mock_chat_fun <- function(...) {
    "br(\"limma\", step_dea_limma(), step_volcano()); step_pca()"
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  bp <- inquire_blueprint("dummy description")

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 3)
  expect_named(bp, c("limma__dea_limma", "limma__volcano", "pca"))
  expect_equal(bp[[1]]$branch, "limma")
  expect_equal(bp[[2]]$branch, "limma")
  expect_null(bp[[3]]$branch)
})

test_that("inquire_blueprint handles single step", {
  skip_on_ci()
  skip_if_not_installed("ellmer")

  mock_chat_fun <- function(...) "step_tsne(on = 'exp')"
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  bp <- inquire_blueprint("dummy description")

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 1)
  expect_equal(bp[[1]]$id, "tsne")
})

test_that("inquire_blueprint cleans AI output (backticks)", {
  skip_on_ci()
  skip_if_not_installed("ellmer")

  mock_chat_fun <- function(...) "`step_preprocess()`"
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  bp <- inquire_blueprint("dummy description")
  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 1)
  expect_equal(bp[[1]]$id, "preprocess")
})

test_that("inquire_blueprint raises error on invalid format", {
  skip_on_ci()
  skip_if_not_installed("ellmer")

  mock_chat_fun <- function(...) "I am not a blueprint"
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(inquire_blueprint("dummy description", max_retries = 0), "Failed to generate a valid blueprint")
})

test_that("inquire_blueprint raises error on empty output", {
  skip_on_ci()
  skip_if_not_installed("ellmer")

  mock_chat_fun <- function(...) ""
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(inquire_blueprint("dummy description", max_retries = 0), "Failed to generate a valid blueprint")
})

test_that("inquire_blueprint raises error on valid format but execution error", {
  skip_on_ci()
  skip_if_not_installed("ellmer")

  # step_nonexistent doesn't exist, so eval() should fail
  mock_chat_fun <- function(...) "step_nonexistent()"
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(inquire_blueprint("dummy description", max_retries = 0), "Failed to generate a valid blueprint")
})

test_that("inquire_blueprint requires API key", {
  # No API key set
  withr::local_envvar(c(DEEPSEEK_API_KEY = ""))
  expect_error(inquire_blueprint("dummy"), "API key for DeepSeek chat model is not set")
})

test_that("inquire_blueprint handles valid output immediately (mocked)", {
  skip_if_not_installed("ellmer")

  call_count <- 0
  mock_chat_fun <- function(...) {
    call_count <<- call_count + 1
    "step_ident_overview(); step_pca()"
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  bp <- inquire_blueprint("test description", max_retries = 2)

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 2)
  expect_named(bp, c("ident_overview", "pca"))
  expect_equal(call_count, 1)
})

test_that("inquire_blueprint retries on invalid output", {
  skip_if_not_installed("ellmer")

  call_count <- 0
  error_feedback_received <- FALSE

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return("step_ident_overview(") # Invalid
    } else {
      # Check if we received the error message in prompt
      if (grepl("Invalid format", prompt) || grepl("Error:", prompt)) {
        error_feedback_received <<- TRUE
      }
      return("step_ident_overview(); step_pca()") # Valid
    }
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  suppressMessages(bp <- inquire_blueprint("test description", max_retries = 2))

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 2)
  expect_equal(call_count, 2)
  expect_true(error_feedback_received)
})

test_that("inquire_blueprint reflects full error details back to LLM", {
  skip_if_not_installed("ellmer")

  call_count <- 0
  prompt_received <- NULL

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return("step_volcano()")
    }
    prompt_received <<- prompt
    "step_dea_ttest(); step_volcano()"
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  suppressMessages(inquire_blueprint("test description", max_retries = 1))

  expect_true(grepl("missing data", prompt_received))
  expect_true(grepl("dea_res", prompt_received))
})

test_that("inquire_blueprint fails after max retries", {
  skip_if_not_installed("ellmer")

  call_count <- 0
  mock_chat_fun <- function(...) {
    call_count <<- call_count + 1
    "invalid_code"
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  expect_error(
    suppressMessages(inquire_blueprint("test description", max_retries = 2)),
    "Failed to generate a valid blueprint after 2 retries"
  )

  # Initial call + 2 retries = 3 calls
  expect_equal(call_count, 3)
})
