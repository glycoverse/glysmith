test_that("quench_result writes README.md based on meta", {
  plots <- list(
    scatter = ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point(),
    scatter2 = ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  )
  tables <- list(
    summary = tibble::tibble(a = 1:3, b = c("x", "y", "z")),
    summary2 = tibble::tibble(a = 4:5, b = c("m", "n"))
  )
  meta <- list(
    explanation = c(
      "plots$scatter" = "Scatter plot of mtcars mpg vs wt.",
      "tables$summary" = "A tiny summary table for testing."
    ),
    steps = c("preprocessing", "pca analysis")
  )

  x <- glysmith_result(exp = list(dummy = TRUE), plots = plots, tables = tables, meta = meta)

  out_dir <- tempfile(pattern = "glysmith-result-")
  if (fs::dir_exists(out_dir)) fs::dir_delete(out_dir)

  quench_result(x, out_dir, plot_ext = "png", table_ext = "csv")

  expect_true(fs::file_exists(fs::path(out_dir, "README.md")))
  expect_true(fs::file_exists(fs::path(out_dir, "experiment.rds")))
  expect_true(fs::file_exists(fs::path(out_dir, "meta.rds")))
  expect_true(fs::file_exists(fs::path(out_dir, "plots", "scatter.png")))
  expect_true(fs::file_exists(fs::path(out_dir, "plots", "scatter2.png")))
  expect_true(fs::file_exists(fs::path(out_dir, "tables", "summary.csv")))
  expect_true(fs::file_exists(fs::path(out_dir, "tables", "summary2.csv")))

  readme <- readLines(fs::path(out_dir, "README.md"), warn = FALSE)
  expect_true(any(grepl("`plots/scatter.png`: Scatter plot of mtcars mpg vs wt.", readme, fixed = TRUE)))
  expect_true(any(grepl("`plots/scatter2.png`: Plot output.", readme, fixed = TRUE)))
  expect_true(any(grepl("`tables/summary.csv`: A tiny summary table for testing.", readme, fixed = TRUE)))
  expect_true(any(grepl("`tables/summary2.csv`: Table output.", readme, fixed = TRUE)))
  expect_true(any(grepl("- preprocessing", readme, fixed = TRUE)))
  expect_true(any(grepl("- pca analysis", readme, fixed = TRUE)))
})
