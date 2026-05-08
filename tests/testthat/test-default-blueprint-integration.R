test_that("default blueprint workflow forges, quenches, and polishes results", {
  skip_if_default_blueprint_integration_disabled()

  bp <- blueprint_default()
  skip_if_glysmith_deps_not_installed(bp)

  exp <- glyexp::real_experiment2
  suppressMessages(result <- forge_analysis(exp, blueprint = bp))

  expect_s3_class(result, "glysmith_result")
  expect_identical(result$blueprint, bp)
  expect_true("exp" %in% names(result$data))
  expect_true(all(
    c(
      "ident_overview",
      "preprocess",
      "pca",
      "dea_limma",
      "derive_traits"
    ) %in%
      result$meta$steps
  ))
  expect_true("summary" %in% names(result$tables))
  expect_true("pca_scores" %in% names(result$plots))

  output_dir <- fs::path(withr::local_tempdir(), "default-blueprint-result")
  suppressWarnings(suppressMessages(
    quench_result(result, output_dir, plot_ext = "png")
  ))

  expect_true(fs::file_exists(fs::path(output_dir, "README.md")))
  expect_true(fs::file_exists(fs::path(output_dir, "experiment.rds")))
  expect_true(fs::file_exists(fs::path(output_dir, "meta.rds")))
  expect_true(fs::dir_exists(fs::path(output_dir, "plots")))
  expect_true(fs::dir_exists(fs::path(output_dir, "tables")))

  report_file <- fs::path(
    withr::local_tempdir(),
    "default-blueprint-report.html"
  )
  suppressMessages(rendered <- polish_report(result, report_file, open = FALSE))

  expect_equal(
    rendered,
    normalizePath(report_file, winslash = "/", mustWork = TRUE)
  )
  expect_true(fs::file_exists(report_file))
  report <- readLines(report_file, warn = FALSE)
  expect_true(any(grepl("GlySmith report", report, fixed = TRUE)))
})
