test_that("blueprint checks ctx$data dependencies", {
  expect_no_error(blueprint(
    step_dea(),
    step_volcano()
  ))
  expect_snapshot(blueprint(step_volcano()), error = TRUE)
})