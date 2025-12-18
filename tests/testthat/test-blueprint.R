test_that("blueprint checks ctx$data dependencies", {
  expect_no_error(blueprint(
    step_dea(),
    step_volcano()
  ))
  expect_snapshot(blueprint(step_volcano()), error = TRUE)
})

test_that("blueprint checks overwrites", {
  steps <- list(
    step("step1", "Step 1", function(ctx) ctx, generate = "x"),
    step("step2", "Step 2", function(ctx) ctx, generate = "x")
  )
  expect_snapshot(blueprint(!!!steps))
})

test_that("blueprint check duplicated steps", {
  expect_snapshot(
    blueprint(
      step_dea(),
      step_dea(),
      step_volcano(),
      step_volcano()
    ),
    error = TRUE
  )
})

test_that("writing and loading blueprint works", {
  bp <- blueprint(
    step_dea(),
    step_volcano()
  )
  file <- tempfile(fileext = ".rds")
  write_blueprint(bp, file)
  expect_s3_class(read_blueprint(file), "glysmith_blueprint")
})