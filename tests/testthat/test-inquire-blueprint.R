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

test_that("inquire_blueprint handles single clarification question from LLM", {
  skip_if_not_installed("ellmer")
  local_mock_glycan_fact()

  call_count <- 0
  prompt_received <- NULL
  pro_expr_path <- withr::local_tempfile(fileext = ".csv")
  writeLines("protein,s1", pro_expr_path)

  mock_chat_fun <- function(prompt) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return('{"question":"What is the path to your protein expression matrix?"}')
    }
    prompt_received <<- prompt
    paste0(
      '{"explanation":"Adjust proteins then PCA.","steps":[',
      '"step_adjust_protein(pro_expr_path = \'', pro_expr_path, '\')",',
      '"step_pca()"]}'
    )
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
    .ask_single_question = function(...) stop("Unexpected clarification request."),
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

test_that("system prompt uses single-question format", {
  prompt <- .inquire_blueprint_sys_prompt()

  # Should mention "one question" or similar single-question language
  expect_true(
    grepl("one question", prompt, ignore.case = TRUE),
    info = "System prompt should mention asking one question at a time"
  )

  # Should show example with "question" field (not "questions" array)
  expect_true(
    grepl('"question":', prompt),
    info = "System prompt should show single question format with 'question' field"
  )

  # Should NOT show "questions" array format (old format)
  expect_false(
    grepl('"questions":\\s*\\[', prompt),
    info = "System prompt should not use 'questions' array format"
  )

  # Example flow should be present
  expect_true(
    grepl("Example flow", prompt),
    info = "System prompt should include example flow"
  )
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

test_that("ask_single_question errors when non-interactive", {
  skip_if(interactive())
  expect_error(glysmith:::.ask_single_question("Question?"), "interactive input")
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

# Tests for helper functions
test_that(".extract_json_output finds JSON without fences", {
  output <- "Some text before\n{\"steps\":[\"step_a()\"]}\nSome text after"
  json_text <- glysmith:::.extract_json_output(output)
  expect_true(grepl("steps", json_text))
})

test_that(".extract_json_output returns NULL for empty output", {
  expect_null(glysmith:::.extract_json_output(""))
  expect_null(glysmith:::.extract_json_output("   "))
})

test_that(".extract_json_output handles code fences with json lang", {
  output <- "```json\n{\"steps\":[\"step_a()\"]}\n```"
  json_text <- glysmith:::.extract_json_output(output)
  expect_true(grepl("steps", json_text))
})

test_that(".parse_step_expr handles backslash escapes", {
  # This should not error - the function handles escape sequences
  expect_error(glysmith:::.parse_step_expr('step_heatmap(on = "sig_exp")'), NA)
})

test_that(".normalize_windows_paths handles basic paths", {
  # Test that it doesn't break normal text
  result <- glysmith:::.normalize_windows_paths("normal text")
  expect_equal(result, "normal text")
})

test_that(".normalize_windows_path normalizes forward slashes", {
  # Forward slashes should stay as-is
  result <- glysmith:::.normalize_windows_path("a/b/c")
  expect_equal(result, as.character(fs::path("a", "b", "c")))
})

test_that(".normalize_windows_path handles double backslashes", {
  # Double backslashes should be converted
  result <- glysmith:::.normalize_windows_path("a\\\\b\\\\c")
  expect_true(grepl("/", result))
})

test_that(".filter_inquire_blueprint_args returns all args", {
  args <- c("on" = "Name of the experiment", "method" = "Computation method")
  result <- glysmith:::.filter_inquire_blueprint_args("step_pca", args)
  expect_equal(names(result), c("on", "method"))
})

test_that(".process_blueprint_response handles JSON with explanation", {
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) {
    '{"explanation":"Preprocess then analyze.","steps":["step_preprocess()"]}'
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy"))
  expect_s3_class(bp, "glysmith_blueprint")
})

test_that(".process_blueprint_response handles semicolon-separated steps", {
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) {
    '{"explanation":"Multiple steps.","steps":["step_preprocess(); step_pca()"]}'
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy"))
  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 2)
})

test_that(".process_blueprint_response handles semicolon at end", {
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) {
    '{"explanation":"Steps with trailing semicolon.","steps":["step_preprocess();"]}'
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy"))
  expect_s3_class(bp, "glysmith_blueprint")
})

test_that(".process_blueprint_response rejects invalid step format", {
  result <- glysmith:::.process_blueprint_response('{"steps":["not_a_step()"]}')
  expect_false(result$valid)
  expect_true(grepl("Invalid format", result$error))
})

test_that(".process_blueprint_response rejects empty steps array", {
  result <- glysmith:::.process_blueprint_response('{"steps":[]}')
  expect_false(result$valid)
  expect_true(grepl("output", tolower(result$error)))
})

test_that(".process_blueprint_response handles quoted steps", {
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) {
    '{"explanation":"Preprocess.","steps":["step_preprocess()"]}'
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy"))
  expect_s3_class(bp, "glysmith_blueprint")
  expect_equal(bp[[1]]$id, "preprocess")
})

test_that(".process_blueprint_response handles step with quoted arguments", {
  local_mock_glycan_fact()

  mock_chat_fun <- function(...) {
    '```json\n{"explanation":"DEA then heatmap.","steps":["step_dea_limma()","step_heatmap(on = \\"sig_exp\\")"]}\n```'
  }
  local_mocked_bindings(
    chat_deepseek = function(...) list(chat = mock_chat_fun),
    .package = "ellmer"
  )

  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_key"))

  suppressMessages(bp <- inquire_blueprint("dummy"))
  expect_s3_class(bp, "glysmith_blueprint")
  expect_length(bp, 2)
})

test_that(".process_blueprint_response rejects non-array steps", {
  # When JSON parses but steps is not an array, it may still be valid if it's a string
  # The function converts semicolon-separated strings to arrays
  result <- glysmith:::.process_blueprint_response('{"steps":123}')
  expect_false(result$valid)
})

test_that(".generate_exp_info returns empty for NULL experiment", {
  result <- glysmith:::.generate_exp_info(NULL, "group")
  expect_equal(result, "")
})

test_that(".get_rd_database returns database or NULL", {
  # This should either return a list or NULL depending on environment
  rd_db <- glysmith:::.get_rd_database()
  # Just check it doesn't error
  expect_true(is.list(rd_db) || is.null(rd_db))
})

test_that(".get_rd_tag finds matching tags", {
  skip_if_not_installed("tools")
  rd_db <- glysmith:::.get_rd_database()
  if (is.null(rd_db)) skip("Rd database not available")

  # Find the preprocess Rd file
  rd <- rd_db[["step_preprocess.Rd"]]
  if (is.null(rd)) skip("step_preprocess.Rd not found")

  tags <- glysmith:::.get_rd_tag(rd, "\\title")
  expect_length(tags, 1)
})

test_that(".get_rd_tag_values extracts alias values", {
  skip_if_not_installed("tools")
  rd_db <- glysmith:::.get_rd_database()
  if (is.null(rd_db)) skip("Rd database not available")

  rd <- rd_db[["step_preprocess.Rd"]]
  if (is.null(rd)) skip("step_preprocess.Rd not found")

  aliases <- glysmith:::.get_rd_tag_values(rd, "\\alias")
  expect_true("step_preprocess" %in% aliases)
})

test_that(".parse_rd_content handles nested structures", {
  result <- glysmith:::.parse_rd_content("simple text")
  expect_equal(result, "simple text")

  # Test with a list
  nested <- list("part1", list("part2"))
  result <- glysmith:::.parse_rd_content(nested)
  expect_equal(result, "part1part2")
})

test_that(".clean_rd_text removes Rd markup", {
  text <- "\\code{some_code} and \\link{some_function}"
  result <- glysmith:::.clean_rd_text(text)
  # Backticks are kept for code formatting
  expect_true(grepl("some_code", result))
  expect_true(grepl("some_function", result))
})

test_that(".get_rd_tag_text extracts title", {
  skip_if_not_installed("tools")
  rd_db <- glysmith:::.get_rd_database()
  if (is.null(rd_db)) skip("Rd database not available")

  rd <- rd_db[["step_preprocess.Rd"]]
  if (is.null(rd)) skip("step_preprocess.Rd not found")

  title <- glysmith:::.get_rd_tag_text(rd, "\\title")
  expect_true(nzchar(title))
})

test_that(".get_rd_section_text extracts AI Prompt section", {
  skip_if_not_installed("tools")
  rd_db <- glysmith:::.get_rd_database()
  if (is.null(rd_db)) skip("Rd database not available")

  rd <- rd_db[["step_correlation.Rd"]]
  if (is.null(rd)) skip("step_correlation.Rd not found")

  ai_text <- glysmith:::.get_rd_section_text(rd, "AI Prompt")
  # AI Prompt may or may not be present
  expect_true(is.character(ai_text))
})

test_that(".get_rd_arguments extracts argument descriptions", {
  skip_if_not_installed("tools")
  rd_db <- glysmith:::.get_rd_database()
  if (is.null(rd_db)) skip("Rd database not available")

  rd <- rd_db[["step_preprocess.Rd"]]
  if (is.null(rd)) skip("step_preprocess.Rd not found")

  args <- glysmith:::.get_rd_arguments(rd)
  # Arguments section returns a named list
  expect_true(is.list(args))
})
