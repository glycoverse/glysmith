result_for_test <- function() {
  exp <- list()
  plots <- list(plot1 = "plot1", plot2 = "plot2")
  tables <- list(table1 = "table1")
  meta <- list()
  data <- list()
  blueprint <- new_blueprint(list(step_preprocess(), step_pca()))
  glysmith_result(exp, data, plots, tables, meta, blueprint)
}

test_that("print.glysmith_result works correctly", {
  result <- result_for_test()
  expect_snapshot(print(result))
})

test_that("print.glysmith_result handles empty plots and tables", {
  exp <- list()
  plots <- list()
  tables <- list()
  meta <- list()
  data <- list()
  blueprint <- new_blueprint(list(step_preprocess(), step_pca()))
  result <- glysmith_result(exp, data, plots, tables, meta, blueprint)

  expect_snapshot(print(result))
})

test_that("print.glysmith_result handles NULL plots and tables", {
  exp <- list()
  plots <- NULL
  tables <- NULL
  meta <- list()
  data <- list()
  blueprint <- new_blueprint(list(step_preprocess(), step_pca()))
  result <- glysmith_result(exp, data, plots, tables, meta, blueprint)

  expect_snapshot(print(result))
})

test_that("cast_table works", {
  result <- result_for_test()
  expect_equal(cast_table(result, "table1"), "table1")
  expect_snapshot(cast_table(result, "table2"), error = TRUE)
})

test_that("cast_plot works", {
  result <- result_for_test()
  expect_equal(cast_plot(result, "plot1"), "plot1")
  expect_snapshot(cast_plot(result, "plot3"), error = TRUE)
})