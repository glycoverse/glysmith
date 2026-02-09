test_that("run_blueprint handles missing dependencies", {
  step_req <- structure(
    list(
      id = "step_req",
      label = "Require Step",
      require = "missing_data",
      run = function(ctx) ctx,
      retry = 0,
      signature = "step_req()"
    ),
    class = "glysmith_step"
  )

  ctx <- list(meta = list(logs = list(), steps = character(0)), data = list())
  bp_req <- glysmith:::new_blueprint(list(step_req))

  expect_message(
    {
      ctx_result <- glysmith:::run_blueprint(bp_req, ctx)
    },
    "Skipping .* due to missing ctx\\$data keys"
  )

  expect_false("step_req" %in% ctx_result$meta$steps)
})

test_that("run_blueprint handles step failure", {
  step_fail <- structure(
    list(
      id = "step_fail",
      label = "Failing Step",
      run = function(ctx) stop("intentionally failed"),
      retry = 0,
      signature = "step_fail()"
    ),
    class = "glysmith_step"
  )

  step_ok <- structure(
    list(
      id = "step_ok",
      label = "OK Step",
      run = function(ctx) {
        ctx$data$ok <- TRUE
        ctx
      },
      retry = 0,
      signature = "step_ok()"
    ),
    class = "glysmith_step"
  )

  ctx <- list(meta = list(logs = list(), steps = character(0)), data = list())
  bp <- glysmith:::new_blueprint(list(step_fail, step_ok))

  # Should not error
  expect_no_error({
    ctx_result <- glysmith:::run_blueprint(bp, ctx, quiet = TRUE)
  })

  # Check logs
  expect_true(!is.null(ctx_result$meta$logs$step_fail$error))
  expect_match(ctx_result$meta$logs$step_fail$error, "intentionally failed")

  # Check next step ran
  expect_true(ctx_result$data$ok)
  expect_true("step_ok" %in% ctx_result$meta$steps)
  # Ensure step_fail is NOT in steps list
  expect_false("step_fail" %in% ctx_result$meta$steps)
})

test_that("warnings from step execution are captured and suppressed", {
  # Create a test step that always issues a warning
  step_with_warning <- step(
    id = "test_warning",
    label = "Test step that issues a warning",
    run = function(ctx) {
      warning("This is a test warning that should be captured")
      ctx
    }
  )

  # Create a blueprint with this step
  bp <- blueprint(step_with_warning)

  # Create a minimal context with exp data
  ctx <- list(
    data = list(exp = glyexp::real_experiment2),
    meta = list()
  )

  # Run the blueprint - NO warnings should propagate to the test output
  # All warnings should be captured in the logs
  expect_no_warning(
    result_ctx <- run_blueprint(bp, ctx, quiet = TRUE)
  )

  # Verify the step ran successfully
  expect_true("test_warning" %in% result_ctx$meta$steps)

  # Verify the warning was captured in the logs
  logs <- result_ctx$meta$logs$test_warning
  expect_true(length(logs$warning) > 0)
  expect_s3_class(logs$warning[[1]], "simpleWarning")
  expect_match(
    conditionMessage(logs$warning[[1]]),
    "This is a test warning that should be captured"
  )
})

test_that("multiple warnings from step execution are all captured", {
  # Create a test step that issues multiple warnings
  step_with_multiple_warnings <- step(
    id = "test_multiple_warnings",
    label = "Test step that issues multiple warnings",
    run = function(ctx) {
      warning("First warning")
      warning("Second warning")
      warning("Third warning")
      ctx
    }
  )

  bp <- blueprint(step_with_multiple_warnings)

  ctx <- list(
    data = list(exp = glyexp::real_experiment2),
    meta = list()
  )

  # No warnings should propagate
  expect_no_warning(
    result_ctx <- run_blueprint(bp, ctx, quiet = TRUE)
  )

  # Verify all three warnings were captured
  logs <- result_ctx$meta$logs$test_multiple_warnings
  expect_equal(length(logs$warning), 3)
  expect_match(conditionMessage(logs$warning[[1]]), "First warning")
  expect_match(conditionMessage(logs$warning[[2]]), "Second warning")
  expect_match(conditionMessage(logs$warning[[3]]), "Third warning")
})

test_that("warnings from nested function calls are captured", {
  # Create a helper function that issues a warning
  helper_with_warning <- function() {
    warning("Warning from nested function")
    return(42)
  }

  # Create a step that calls the helper
  step_with_nested_warning <- step(
    id = "test_nested_warning",
    label = "Test step with nested warning",
    run = function(ctx) {
      result <- helper_with_warning()
      ctx$data$result <- result
      ctx
    }
  )

  bp <- blueprint(step_with_nested_warning)

  ctx <- list(
    data = list(exp = glyexp::real_experiment2),
    meta = list()
  )

  # No warnings should propagate from nested calls either
  expect_no_warning(
    result_ctx <- run_blueprint(bp, ctx, quiet = TRUE)
  )

  # Verify the warning was captured
  logs <- result_ctx$meta$logs$test_nested_warning
  expect_true(length(logs$warning) > 0)
  expect_match(
    conditionMessage(logs$warning[[1]]),
    "Warning from nested function"
  )

  # Verify the function still executed correctly
  expect_equal(result_ctx$data$result, 42)
})

test_that("branch steps namespace outputs", {
  step_make <- step(
    id = "make",
    label = "Make",
    run = function(ctx) {
      ctx_add_data(ctx, "x", 1)
    },
    generate = "x"
  )

  step_use <- step(
    id = "use",
    label = "Use",
    run = function(ctx) {
      val <- ctx_get_data(ctx, "x")
      ctx_add_data(ctx, "y", val + 1)
    },
    require = "x",
    generate = "y"
  )

  bp <- blueprint(
    br("one", step_make, step_use),
    br("two", step_make, step_use)
  )

  ctx <- list(
    data = list(exp = 1),
    plots = list(),
    tables = list(),
    meta = list()
  )
  ctx <- run_blueprint(bp, ctx, quiet = TRUE)

  expect_true(all(
    c("one__x", "one__y", "two__x", "two__y") %in% names(ctx$data)
  ))
  expect_false("x" %in% names(ctx$data))
  expect_equal(ctx$data[["one__y"]], 2)
  expect_equal(ctx$data[["two__y"]], 2)
})

test_that("branch steps can inherit global data", {
  step_global <- step(
    id = "global",
    label = "Global Step",
    run = function(ctx) {
      ctx_add_data(ctx, "uniq_global_val", 999)
    },
    generate = "uniq_global_val"
  )

  step_branch <- step(
    id = "branch",
    label = "Branch Step",
    run = function(ctx) {
      # This step requires "uniq_global_val"
      # It should find it even though it's not prefixed with the branch name
      val <- ctx_get_data(ctx, "uniq_global_val")
      ctx_add_data(ctx, "branch_res", val + 1)
    },
    require = "uniq_global_val",
    generate = "branch_res"
  )

  bp <- blueprint(
    step_global,
    br("b1", step_branch)
  )

  ctx <- list(
    data = list(exp = 1),
    plots = list(),
    tables = list(),
    meta = list()
  )

  # This should pass without error
  expect_no_error(ctx <- run_blueprint(bp, ctx, quiet = TRUE))

  # Check output
  expect_true("b1__branch_res" %in% names(ctx$data))
  expect_equal(ctx$data[["b1__branch_res"]], 1000)
})
