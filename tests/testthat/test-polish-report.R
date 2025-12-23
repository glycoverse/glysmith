test_that("polish_report works", {
  exp <- glyexp::real_experiment2
  suppressMessages(result <- forge_analysis(exp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  polish_report(result, output_file, open = FALSE)
  expect_true(fs::file_exists(output_file))
})