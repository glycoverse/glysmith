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
  skip_on_cran()
  local_mocked_bindings(.ask_ai = function(system_prompt, user_prompt, ...) {
    if (grepl("report section organizer", system_prompt, fixed = TRUE)) {
      return(paste(
        "## Preprocessing",
        "items: step:preprocess",
        "## DEA analysis",
        "items: step:dea_limma; plot:volcano; plot:heatmap",
        sep = "\n"
      ))
    }
    "AI response"
  })
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_api_key"))
  bp <- structure(
    list(
      preprocess = list(
        id = "preprocess",
        label = "Preprocessing",
        report = function(x) "Preprocess report."
      ),
      dea_limma = list(
        id = "dea_limma",
        label = "DEA analysis (limma)",
        report = function(x) "DEA report."
      ),
      volcano = list(
        id = "volcano",
        label = "Volcano plot",
        report = function(x) "Volcano report."
      ),
      heatmap = list(
        id = "heatmap",
        label = "Heatmap",
        report = function(x) "Heatmap report."
      )
    ),
    class = "glysmith_blueprint"
  )
  result <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(
        volcano = ggplot2::ggplot(),
        heatmap = ggplot2::ggplot()
      ),
      tables = list(),
      meta = list(
        steps = c("preprocess", "dea_limma", "volcano", "heatmap"),
        explanation = list(
          "plots$volcano" = "Volcano plot.",
          "plots$heatmap" = "Heatmap plot."
        )
      ),
      blueprint = bp
    ),
    class = "glysmith_result"
  )
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  suppressMessages(polish_report(result, output_file, open = FALSE, use_ai = TRUE))
  expect_true(fs::file_exists(output_file))
  output_lines <- readLines(output_file)
  expect_true(any(grepl("Preprocessing", output_lines, fixed = TRUE)))
  expect_true(any(grepl("DEA analysis", output_lines, fixed = TRUE)))
  expect_true(any(grepl("AI response", output_lines, fixed = TRUE)))
})

test_that("polish_report raises an error when API key is not set", {
  skip_on_ci()
  skip_on_cran()
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
  skip_on_cran()
  local_mocked_bindings(.ask_ai = function(...) stop("AI error"))
  exp <- glyexp::real_experiment2
  bp <- blueprint(step_preprocess())
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_api_key"))
  suppressMessages(result <- forge_analysis(exp, blueprint = bp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  warn_msgs <- character(0)
  suppressMessages(withCallingHandlers(
    polish_report(result, output_file, open = FALSE, use_ai = TRUE),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ))
  expect_true(any(grepl("AI error", warn_msgs, fixed = TRUE)))
})

test_that("polish_report omits empty step placeholders and humanizes plot titles", {
  bp <- structure(
    list(
      volcano = list(
        id = "volcano",
        label = "Volcano plot",
        report = function(x) ""
      ),
      heatmap_sig = list(
        id = "heatmap_sig",
        label = "Heatmap of significant variables",
        report = function(x) ""
      )
    ),
    class = "glysmith_blueprint"
  )
  result <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(
        volcano_H_vs_M = ggplot2::ggplot(),
        heatmap_sig = ggplot2::ggplot()
      ),
      tables = list(),
      meta = list(steps = c("volcano", "heatmap_sig"), explanation = list()),
      blueprint = bp
    ),
    class = "glysmith_result"
  )
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  suppressMessages(polish_report(result, output_file, open = FALSE))
  output_lines <- readLines(output_file)
  expect_false(any(grepl("No report content for this step.", output_lines, fixed = TRUE)))
  expect_true(any(grepl("Volcano plot: H vs M", output_lines, fixed = TRUE)))
  expect_true(any(grepl("Heatmap of significant variables", output_lines, fixed = TRUE)))
})
