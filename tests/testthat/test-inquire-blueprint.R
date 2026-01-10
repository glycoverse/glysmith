test_that("inquire_blueprint works with valid AI output", {
  skip_on_ci()
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  # Mock `ellmer::chat_deepseek` to return a chat object with a mocked `chat` method
  mock_chat_fun <- function(...) {
    '{"explanation":"Preprocess then run PCA.","steps":["step_preprocess()","step_pca()"]}'
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy description"))

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 2)
  expect_equal(bp[[1]]$id, "preprocess")
  expect_equal(bp[[2]]$id, "pca")
})

test_that("inquire_blueprint supports branches", {
  skip_on_ci()
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) {
    paste0(
      '{"explanation":"Compare limma and PCA.","steps":["br(\\"limma\\", step_dea_limma(), step_volcano())",',
      '"step_pca()"]}'
    )
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy description"))

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
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) {
    '{"explanation":"Run t-SNE.","steps":["step_tsne(on = \\"exp\\")"]}'
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy description"))

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 1)
  expect_equal(bp[[1]]$id, "tsne")
})

test_that("inquire_blueprint cleans AI output (code fences)", {
  skip_on_ci()
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) {
    "```json\n{\"explanation\":\"Preprocess.\",\"steps\":[\"step_preprocess()\"]}\n```"
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy description"))
  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 1)
  expect_equal(bp[[1]]$id, "preprocess")
})

test_that("inquire_blueprint raises error on invalid format", {
  skip_on_ci()
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) "I am not a blueprint"
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(
    suppressMessages(inquire_blueprint("dummy description", max_retries = 0)),
    "Failed to generate a valid blueprint"
  )
})

test_that("inquire_blueprint raises error on empty output", {
  skip_on_ci()
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) ""
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(
    suppressMessages(inquire_blueprint("dummy description", max_retries = 0)),
    "Failed to generate a valid blueprint"
  )
})

test_that("inquire_blueprint raises error on valid format but execution error", {
  skip_on_ci()
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  # step_nonexistent doesn't exist, so eval() should fail
  mock_chat_fun <- function(...) {
    '{"explanation":"Try a nonexistent step.","steps":["step_nonexistent()"]}'
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  expect_error(
    suppressMessages(inquire_blueprint("dummy description", max_retries = 0)),
    "Failed to generate a valid blueprint"
  )
})

test_that("inquire_blueprint requires API key", {
  # No API key set
  withr::local_envvar(c(DEEPSEEK_API_KEY = ""))
  expect_error(
    suppressMessages(inquire_blueprint("dummy")),
    "API key for DeepSeek chat model is not set"
  )
})

test_that("inquire_blueprint handles valid output immediately (mocked)", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  call_count <- 0
  mock_chat_fun <- function(...) {
    call_count <<- call_count + 1
    '{"explanation":"Overview then PCA.","steps":["step_ident_overview()","step_pca()"]}'
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  suppressMessages(bp <- inquire_blueprint("test description", max_retries = 2))

  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 2)
  expect_named(bp, c("ident_overview", "pca"))
  expect_equal(call_count, 1)
})

test_that("inquire_blueprint retries on invalid output", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  call_count <- 0
  error_feedback_received <- FALSE

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return("{\"explanation\":\"Broken JSON\",\"steps\":[\"step_ident_overview(\"]}") # Invalid
    } else {
      # Check if we received the error message in prompt
      if (grepl("Invalid format", prompt) || grepl("Error:", prompt) || grepl("Invalid JSON", prompt)) {
        error_feedback_received <<- TRUE
      }
      return("{\"explanation\":\"Valid JSON\",\"steps\":[\"step_ident_overview()\",\"step_pca()\"]}")
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
  local_mock_glycan_fact()

  call_count <- 0
  prompt_received <- NULL

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return("{\"explanation\":\"Volcano only.\",\"steps\":[\"step_volcano()\"]}")
    }
    prompt_received <<- prompt
    "{\"explanation\":\"DEA then volcano.\",\"steps\":[\"step_dea_ttest()\",\"step_volcano()\"]}"
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
  local_mock_glycan_fact()

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

test_that("inquire_blueprint handles clarification questions from LLM", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  call_count <- 0
  prompt_received <- NULL
  pro_expr_path <- withr::local_tempfile(fileext = ".csv")
  writeLines("protein,s1", pro_expr_path)

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return(paste0(
        "{\"questions\":[",
        "\"What is the path to your protein expression matrix?\",",
        "\"If you haven't prepared it yet, create a CSV/TSV with protein accessions in the first column and sample names as columns, or an RDS with a matrix/data.frame using row names for accessions and columns for samples.\"",
        "]}"
      ))
    }
    prompt_received <<- prompt
    paste0(
      "{\"explanation\":\"Adjust proteins then PCA.\",\"steps\":[",
      "\"step_adjust_protein(pro_expr_path = '", pro_expr_path, "')\",",
      "\"step_pca()\"]}"
    )
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

  suppressMessages(bp <- inquire_blueprint("test description", max_retries = 1))

  expect_s3_class(bp, "glysmith_blueprint")
  expect_equal(call_count, 2)
  expect_true(grepl(pro_expr_path, prompt_received, fixed = TRUE))
})

test_that("inquire_blueprint does not auto-ask missing step arguments", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  call_count <- 0
  prompt_received <- NULL

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    prompt_received <<- prompt
    "{\"explanation\":\"Adjust proteins then PCA.\",\"steps\":[\"step_adjust_protein()\",\"step_pca()\"]}"
  }

  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  local_mocked_bindings(
    .ask_inquiry_questions = function(...) stop("Unexpected clarification request."),
    .package = "glysmith"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "fake-key"))

  suppressMessages(bp <- inquire_blueprint("test description", max_retries = 1))

  expect_s3_class(bp, "glysmith_blueprint")
  expect_equal(call_count, 1)
  expect_true(nzchar(prompt_received))
  expect_equal(bp[[1]]$id, "adjust_protein")
  expect_equal(bp[[2]]$id, "pca")
})

test_that("inquire_blueprint includes step parameters in system prompt", {
  prompt <- .inquire_blueprint_sys_prompt()

  block <- regmatches(
    prompt,
    regexpr("- `step_heatmap`[\\s\\S]*?(?=- `step_|$)", prompt, perl = TRUE)
  )

  expect_true(length(block) > 0)
  expect_true(grepl("PARAMETER:", block))
  expect_true(grepl("`on`", block))
})

test_that("extract_inquiry_questions parses list and inline formats", {
  list_output <- "QUESTIONS:\n- first question\n- second question"
  questions <- glysmith:::.extract_inquiry_questions(list_output)
  expect_equal(questions, c("first question", "second question"))

  inline_output <- "QUESTIONS: provide the group column"
  inline <- glysmith:::.extract_inquiry_questions(inline_output)
  expect_equal(inline, "provide the group column")

  expect_null(glysmith:::.extract_inquiry_questions("No questions here"))
})

test_that("format_inquiry_answers formats question and answer pairs", {
  formatted <- glysmith:::.format_inquiry_answers(c("Q1", "Q2"), c("A1", "A2"))
  expect_true(grepl("- Q: Q1", formatted, fixed = TRUE))
  expect_true(grepl("A: A2", formatted, fixed = TRUE))
})

test_that("ask_inquiry_questions errors when non-interactive", {
  skip_if(interactive())
  expect_error(glysmith:::.ask_inquiry_questions("Question?"), "interactive input")
})

test_that("ask_blueprint_review uses readline prompt", {
  prompt_text <- NULL
  local_mocked_bindings(
    readline = function(prompt) {
      prompt_text <<- prompt
      ""
    },
    .package = "base"
  )

  expect_equal(suppressMessages(glysmith:::.ask_blueprint_review()), "")
  expect_true(grepl("Press ENTER", prompt_text, fixed = TRUE))
})

test_that("review_blueprint accepts blueprint on empty input", {
  bp <- structure(list(), class = "glysmith_blueprint")
  ask_count <- 0

  local_mocked_bindings(
    .is_interactive = function() TRUE,
    .print_blueprint_explanation = function(...) NULL,
    .ask_blueprint_review = function() {
      ask_count <<- ask_count + 1
      ""
    },
    modify_blueprint = function(...) stop("Unexpected modification."),
    .package = "glysmith"
  )

  result <- suppressMessages(
    glysmith:::.review_blueprint(
      bp,
      explanation = "desc",
      exp = NULL,
      group_col = "group",
      model = "deepseek-reasoner",
      max_retries = 1
    )
  )

  expect_identical(result, bp)
  expect_equal(ask_count, 1)
})

test_that("review_blueprint applies multiple refinements", {
  bp <- structure(list(), class = "glysmith_blueprint")
  bp_one <- structure(list(list(id = "one")), class = "glysmith_blueprint")
  bp_two <- structure(list(list(id = "two")), class = "glysmith_blueprint")
  responses <- c("add pca", "add heatmap", "")
  call_count <- 0

  local_mocked_bindings(
    .is_interactive = function() TRUE,
    .print_blueprint_explanation = function(...) NULL,
    .ask_blueprint_review = function() {
      response <- responses[[1]]
      responses <<- responses[-1]
      response
    },
    modify_blueprint = function(bp, description, exp, group_col, model, max_retries) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        expect_equal(description, "add pca")
        return(bp_one)
      }
      expect_equal(description, "add heatmap")
      bp_two
    },
    .package = "glysmith"
  )

  result <- suppressMessages(
    glysmith:::.review_blueprint(
      bp,
      explanation = "desc",
      exp = NULL,
      group_col = "group",
      model = "deepseek-reasoner",
      max_retries = 1
    )
  )

  expect_identical(result, bp_two)
  expect_equal(call_count, 2)
})

# Tests for .summarize_tibble
test_that(".summarize_tibble handles NULL and empty tibbles", {
  expect_equal(glysmith:::.summarize_tibble(NULL, "test"), "- test: (empty)")
  expect_equal(glysmith:::.summarize_tibble(data.frame(), "test"), "- test: (empty)")
})

test_that(".summarize_tibble returns formatted output with glimpse-like content", {
  tbl <- data.frame(
    id = 1:3,
    name = c("A", "B", "C"),
    value = c(1.5, 2.5, 3.5)
  )

  result <- glysmith:::.summarize_tibble(tbl, "my_data")

  expect_true(grepl("- my_data \\(3 rows, 3 columns\\)", result))
  expect_true(grepl("\\$ id", result))
  expect_true(grepl("\\$ name", result))
  expect_true(grepl("\\$ value", result))
})

test_that(".summarize_tibble highlights group column", {
  tbl <- data.frame(
    group = c("A", "B", "A"),
    value = c(1, 2, 3)
  )

  result <- glysmith:::.summarize_tibble(tbl, "sample_info", highlight_col = "group")

  expect_true(grepl("\\[GROUP COLUMN\\]", result))
  expect_true(grepl("\\$ group \\[GROUP COLUMN\\]", result))
})

test_that(".summarize_tibble handles different column types", {
  tbl <- data.frame(
    int_col = 1:5,
    dbl_col = c(1.1, 2.2, 3.3, 4.4, 5.5),
    chr_col = letters[1:5],
    lgl_col = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    fct_col = factor(c("a", "b", "a", "b", "a"))
  )

  result <- glysmith:::.summarize_tibble(tbl, "test_data")

  expect_true(grepl("- test_data \\(5 rows, 5 columns\\)", result))
  expect_true(grepl("\\$ int_col", result))
  expect_true(grepl("\\$ dbl_col", result))
  expect_true(grepl("\\$ chr_col", result))
  expect_true(grepl("\\$ lgl_col", result))
  expect_true(grepl("\\$ fct_col", result))
})
