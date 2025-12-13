test_that("print.glysmith_result works correctly", {
  # Create a mock glysmith_result object
  exp <- list()
  plots <- list(plot1 = "plot1", plot2 = "plot2")
  tables <- list(table1 = "table1")
  meta <- list()
  blueprint <- new_blueprint(list(step_preprocess(), step_pca()))
  result <- glysmith_result(exp, plots, tables, meta, blueprint)

  expect_snapshot(print(result))
})

test_that("print.glysmith_result handles empty plots and tables", {
  exp <- list()
  plots <- list()
  tables <- list()
  meta <- list()
  blueprint <- new_blueprint(list(step_preprocess(), step_pca()))
  result <- glysmith_result(exp, plots, tables, meta, blueprint)

  expect_snapshot(print(result))
})

test_that("print.glysmith_result handles NULL plots and tables", {
  exp <- list()
  plots <- NULL
  tables <- NULL
  meta <- list()
  blueprint <- new_blueprint(list(step_preprocess(), step_pca()))
  result <- glysmith_result(exp, plots, tables, meta, blueprint)

  expect_snapshot(print(result))
})
