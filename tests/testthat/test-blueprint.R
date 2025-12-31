test_that("blueprint checks ctx$data dependencies", {
  expect_no_error(blueprint(
    step_dea_limma(),
    step_volcano()
  ))
  # Use expect_error instead of expect_snapshot to avoid message capture differences
  # between test() and R CMD check environments
  my_step <- step("my_step", "My Step", function(ctx) ctx, require = "dea_res")
  expect_error(
    blueprint(my_step),
    class = "rlang_error"
  )
})

test_that("blueprint checks overwrites", {
  step1 <- step("step1", "Step 1", function(ctx) ctx, generate = "x")
  step2 <- step("step2", "Step 2", function(ctx) ctx, generate = "x")
  expect_snapshot(blueprint(step1, step2))
})

test_that("blueprint check duplicated steps", {
  step1 <- step("step1", "Step 1", function(ctx) ctx, generate = "x")
  step2 <- step("step2", "Step 2", function(ctx) ctx, require = "x")
  expect_error(
    blueprint(step1, step1, step2, step2),
    class = "rlang_error"
  )
})

test_that("writing and loading blueprint works", {
  bp <- blueprint(
    step_dea_limma(),
    step_volcano()
  )
  file <- tempfile(fileext = ".rds")
  write_blueprint(bp, file)
  expect_s3_class(read_blueprint(file), "glysmith_blueprint")
})