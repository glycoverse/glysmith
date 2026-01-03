test_that("modify_blueprint includes current blueprint in prompt", {
  skip_if_not_installed("ellmer")

  bp <- blueprint(
    step_preprocess(),
    step_pca()
  )

  prompt_received <- NULL
  mock_chat_fun <- function(prompt) {
    prompt_received <<- prompt
    "step_preprocess(); step_pca(); step_heatmap()"
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  bp_updated <- modify_blueprint(bp, "add a heatmap")

  expect_true(grepl("step_preprocess\\(\\)", prompt_received))
  expect_true(grepl("step_pca\\(\\)", prompt_received))
  expect_s3_class(bp_updated, "glysmith_blueprint")
  expect_length(bp_updated, 3)
})

test_that("modify_blueprint retries on invalid output", {
  skip_if_not_installed("ellmer")

  bp <- blueprint(step_preprocess())
  call_count <- 0
  error_feedback_received <- FALSE

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return("step_heatmap(")
    }
    if (grepl("Invalid format", prompt) || grepl("Error:", prompt)) {
      error_feedback_received <<- TRUE
    }
    "step_preprocess(); step_pca()"
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  suppressMessages(modify_blueprint(bp, "add pca", max_retries = 1))

  expect_equal(call_count, 2)
  expect_true(error_feedback_received)
})

test_that("modify_blueprint requires API key", {
  bp <- blueprint(step_preprocess())
  withr::local_envvar(c(DEEPSEEK_API_KEY = ""))
  expect_error(modify_blueprint(bp, "dummy"), "API key for DeepSeek chat model is not set")
})
