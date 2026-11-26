result_for_test <- function() {
  exp <- list()
  plots <- list(plot1 = "plot1", plot2 = "plot2")
  tables <- list(table1 = "table1")
  meta <- list()
  data <- list(data1 = "data1", data2 = "data2")
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

test_that("cast_data works", {
  result <- result_for_test()
  expect_equal(cast_data(result, "data1"), "data1")
})

test_that("cast_plot without name returns names", {
  result <- result_for_test()
  expect_equal(cast_plot(result), c("plot1", "plot2"))
})


test_that("cast_plot unwraps plots stored with export dimensions", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point()
  ctx <- new_ctx(list(), "group")
  ctx <- ctx_add_plot(ctx, "scatter", plot, width = 7, height = 4)
  result <- result_for_test()
  result$plots <- ctx$plots

  expect_identical(cast_plot(result, "scatter"), plot)
  expect_no_error(cast_plot(result, "scatter") + ggplot2::theme_minimal())
  expect_identical(cast_plot(result), "scatter")
})

test_that("cast_plot preserves legacy plots without metadata wrappers", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt))
  result <- result_for_test()
  result$plots <- list(scatter = plot)

  expect_identical(cast_plot(result, "scatter"), plot)
})
