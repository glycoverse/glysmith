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

test_that("modify_blueprint accepts new and legacy glyco containers", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  prompts <- character()
  mock_chat_fun <- function(prompt) {
    prompts <<- c(prompts, prompt)
    paste0(
      '{"explanation":"Add PCA.",',
      '"steps":["step_preprocess()","step_pca()"]}'
    )
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))
  bp <- blueprint(step_preprocess())

  results <- lapply(test_glysmith_containers(), function(exp) {
    suppressMessages(modify_blueprint(bp, "Add PCA.", exp = exp))
  })

  expect_true(all(vapply(results, inherits, logical(1), "glysmith_blueprint")))
  expect_match(prompts[[1]], "Experiment type: glycomics", fixed = TRUE)
  expect_match(prompts[[2]], "Experiment type: glycoproteomics", fixed = TRUE)
  expect_match(prompts[[3]], "Experiment type: glycomics", fixed = TRUE)
})

test_that("modify_blueprint can use a non-DeepSeek provider", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  bp <- blueprint(step_preprocess())

  captured <- list()
  mock_chat_fun <- function(prompt) {
    captured$prompt <<- prompt
    "{\"explanation\":\"Add PCA.\",\"steps\":[\"step_preprocess()\",\"step_pca()\"]}"
  }

  local_mocked_bindings(
    chat_anthropic = function(system_prompt, model, echo, credentials) {
      captured$model <<- model
      captured$key <<- credentials()
      list(chat = mock_chat_fun)
    },
    .package = "ellmer"
  )

  withr::local_envvar(c(ANTHROPIC_API_KEY = "anthropic-key"))

  suppressMessages(
    bp_updated <- modify_blueprint(
      bp,
      "add pca",
      provider = "anthropic",
      model = "claude-test"
    )
  )

  expect_s3_class(bp_updated, "glysmith_blueprint")
  expect_equal(captured$model, "claude-test")
  expect_equal(captured$key, "anthropic-key")
  expect_true(grepl("step_preprocess\\(\\)", captured$prompt))
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
    if (
      grepl("Invalid format", prompt) ||
        grepl("Error:", prompt) ||
        grepl("Invalid JSON", prompt)
    ) {
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

test_that("modify_blueprint prints retry feedback only for invalid outputs", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  bp <- blueprint(step_preprocess())
  call_count <- 0
  answers <- c("exploring structural features", "all four groups")

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    switch(
      call_count,
      "invalid_code",
      '{"question":"What is your main research objective?"}',
      '{"question":"Which groups do you want to compare?"}',
      '{"explanation":"Add PCA.","steps":["step_preprocess()","step_pca()"]}'
    )
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  local_mocked_bindings(
    .ask_single_question = function(question) {
      answer <- answers[[1]]
      answers <<- answers[-1]
      answer
    },
    .package = "glysmith"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  messages <- character(0)
  withCallingHandlers(
    bp_updated <- modify_blueprint(bp, "add pca", max_retries = 2),
    message = function(cnd) {
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )

  expect_s3_class(bp_updated, "glysmith_blueprint")
  expect_equal(call_count, 4)
  expect_equal(sum(grepl("Retrying with feedback", messages)), 1)
})

test_that("modify_blueprint requires API key", {
  bp <- blueprint(step_preprocess())
  withr::local_envvar(c(DEEPSEEK_API_KEY = ""))
  expect_error(
    suppressMessages(modify_blueprint(bp, "dummy")),
    "API key for DeepSeek chat model is not set"
  )
})

test_that("modify_blueprint handles single clarification question from LLM", {
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
      return('{"question":"What is the path to the protein expression file?"}')
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
    .ask_single_question = function(question) {
      pro_expr_path
    },
    .package = "glysmith"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  suppressMessages(
    bp_updated <- modify_blueprint(
      bp,
      "add protein adjustment",
      max_retries = 1
    )
  )

  expect_s3_class(bp_updated, "glysmith_blueprint")
  expect_equal(call_count, 2)
  expect_true(grepl(pro_expr_path, prompt_received, fixed = TRUE))
})
