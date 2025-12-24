test_that("polish_report works", {
  exp <- glyexp::real_experiment2
  suppressMessages(result <- forge_analysis(exp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  polish_report(result, output_file, open = FALSE)
  expect_true(fs::file_exists(output_file))
})

test_that("polish_report works with AI polish", {
  skip_on_ci()
  local_mocked_bindings(.ask_ai = function(...) "AI response")
  exp <- glyexp::real_experiment2
  bp <- blueprint(step_preprocess())
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_api_key"))
  suppressMessages(result <- forge_analysis(exp, blueprint = bp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  suppressMessages(polish_report(result, output_file, open = FALSE, use_ai = TRUE))
  expect_true(fs::file_exists(output_file))
  expect_true(any(grepl("AI response", readLines(output_file))))
})

test_that("polish_report raises an error when API key is not set", {
  skip_on_ci()
  local_mocked_bindings(.ask_ai = function(...) "AI response")
  exp <- glyexp::real_experiment2
  bp <- blueprint(step_preprocess())
  withr::local_envvar(c(DEEPSEEK_API_KEY = ""))
  suppressMessages(result <- forge_analysis(exp, blueprint = bp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  expect_error(polish_report(result, output_file, open = FALSE, use_ai = TRUE))
})

test_that("polish_report captures AI errors", {
  skip_on_ci()
  local_mocked_bindings(.ask_ai = function(...) stop("AI error"))
  exp <- glyexp::real_experiment2
  bp <- blueprint(step_preprocess())
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_api_key"))
  suppressMessages(result <- forge_analysis(exp, blueprint = bp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  suppressMessages(expect_warning(polish_report(result, output_file, open = FALSE, use_ai = TRUE), "AI error"))
})