minimal_quench_result <- function() {
  glysmith_result(
    exp = list(),
    data = list(),
    plots = list(),
    tables = list(),
    meta = list(),
    blueprint = structure(list(), class = "glysmith_blueprint")
  )
}

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
    steps = c("preprocessing", "pca")
  )
  blueprint <- new_blueprint(list(
    step_preprocess(),
    step_pca()
  ))
  data <- list()

  x <- glysmith_result(
    exp = list(dummy = TRUE),
    plots = plots,
    tables = tables,
    meta = meta,
    data = data,
    blueprint = blueprint
  )

  out_dir <- tempfile(pattern = "glysmith-result-")
  if (fs::dir_exists(out_dir)) fs::dir_delete(out_dir)

  expect_snapshot(
    quench_result(x, out_dir, plot_ext = "png", table_ext = "csv"),
    transform = function(x) stringr::str_replace(x, "'.*'", "'<DIR_PATH>'")
  )

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
  expect_true(any(grepl("- pca", readme, fixed = TRUE)))
})

test_that("quench_result cancels when overwrite is declined", {
  x <- minimal_quench_result()
  out_dir <- fs::path(withr::local_tempdir(), "quench-out")
  fs::dir_create(out_dir)
  sentinel <- fs::path(out_dir, "keep.txt")
  writeLines("keep", sentinel)

  local_mocked_bindings(.ask_overwrite_dir = function() "n", .package = "glysmith")
  expect_error(quench_result(x, out_dir), "Operation cancelled")
  expect_true(fs::file_exists(sentinel))
})

test_that("quench_result overwrites existing directory on confirmation", {
  x <- minimal_quench_result()
  out_dir <- fs::path(withr::local_tempdir(), "quench-out")
  fs::dir_create(out_dir)
  sentinel <- fs::path(out_dir, "old.txt")
  writeLines("old", sentinel)

  local_mocked_bindings(.ask_overwrite_dir = function() "y", .package = "glysmith")
  quench_result(x, out_dir)

  expect_false(fs::file_exists(sentinel))
  expect_true(fs::file_exists(fs::path(out_dir, "README.md")))
})

test_that("quench_result rejects invalid overwrite input", {
  x <- minimal_quench_result()
  out_dir <- fs::path(withr::local_tempdir(), "quench-out")
  fs::dir_create(out_dir)
  sentinel <- fs::path(out_dir, "keep.txt")
  writeLines("keep", sentinel)

  local_mocked_bindings(.ask_overwrite_dir = function() "maybe", .package = "glysmith")
  expect_error(quench_result(x, out_dir), "Invalid input")
  expect_true(fs::file_exists(sentinel))
})

test_that("write_result_readme falls back when explanations are empty", {
  x <- glysmith_result(
    exp = list(),
    data = list(),
    plots = list(plot_a = "plot"),
    tables = list(tab_b = data.frame(a = 1)),
    meta = list(
      explanation = c("plots$plot_a" = "", "tables$tab_b" = NA_character_),
      steps = NULL
    ),
    blueprint = structure(list(), class = "glysmith_blueprint")
  )
  out_dir <- withr::local_tempdir()
  glysmith:::.write_result_readme(x, out_dir, plot_ext = "png", table_ext = "csv")

  readme <- readLines(fs::path(out_dir, "README.md"), warn = FALSE)
  expect_true(any(grepl("`plots/plot_a.png`: Plot output.", readme, fixed = TRUE)))
  expect_true(any(grepl("`tables/tab_b.csv`: Table output.", readme, fixed = TRUE)))
})

test_that("quietly suppresses selected warnings and returns results", {
  expect_equal(glysmith:::.quietly(1 + 1), 2)
  expect_no_warning(glysmith:::.quietly(warning("Ignoring empty aesthetic: x")))
  expect_warning(glysmith:::.quietly(warning("Other warning")), "Other warning")
  expect_error(glysmith:::.quietly(stop("Boom")), "Boom")
})
