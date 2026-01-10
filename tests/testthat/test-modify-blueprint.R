test_that("modify_blueprint includes current blueprint in prompt", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  bp <- blueprint(
    step_preprocess(),
    step_pca()
  )

  prompt_received <- NULL
  mock_chat_fun <- function(prompt) {
    prompt_received <<- prompt
    "{\"explanation\":\"Add heatmap.\",\"steps\":[\"step_preprocess()\",\"step_pca()\",\"step_heatmap()\"]}"
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  suppressMessages(bp_updated <- modify_blueprint(bp, "add a heatmap"))

  expect_true(grepl("step_preprocess\\(\\)", prompt_received))
  expect_true(grepl("step_pca\\(\\)", prompt_received))
  expect_s3_class(bp_updated, "glysmith_blueprint")
  expect_length(bp_updated, 3)
})

test_that("modify_blueprint retries on invalid output", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  bp <- blueprint(step_preprocess())
  call_count <- 0
  error_feedback_received <- FALSE

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return("{\"explanation\":\"Broken JSON\",\"steps\":[\"step_heatmap(\"]}")
    }
    if (grepl("Invalid format", prompt) || grepl("Error:", prompt) || grepl("Invalid JSON", prompt)) {
      error_feedback_received <<- TRUE
    }
    "{\"explanation\":\"Valid JSON\",\"steps\":[\"step_preprocess()\",\"step_pca()\"]}"
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
  expect_error(
    suppressMessages(modify_blueprint(bp, "dummy")),
    "API key for DeepSeek chat model is not set"
  )
})

test_that("modify_blueprint handles clarification questions from LLM", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  bp <- blueprint(step_preprocess())

  call_count <- 0
  prompt_received <- NULL
  pro_expr_path <- "~/path/to/file.csv"

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    prompt_received <<- prompt
    if (call_count == 1) {
      return('{"questions":["What is the path to the protein expression file?"]}')
    }
    # Second call should include the clarification
    expect_true(grepl(pro_expr_path, prompt, fixed = TRUE))
    '{"explanation":"Add protein adjustment step.","steps":["step_preprocess()","step_adjust_protein()"]}'
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  local_mocked_bindings(
    .ask_inquiry_questions = function(questions) {
      list(questions = questions, answers = pro_expr_path)
    },
    .package = "glysmith"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  suppressMessages(bp_updated <- modify_blueprint(bp, "add protein adjustment", max_retries = 1))

  expect_s3_class(bp_updated, "glysmith_blueprint")
  expect_equal(call_count, 2)
  expect_true(grepl(pro_expr_path, prompt_received, fixed = TRUE))
})
