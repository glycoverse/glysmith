test_that("run_blueprint handles missing dependencies", {
  step_req <- structure(
    list(
      id = "step_req",
      label = "Require Step",
      require = "missing_data",
      run = function(ctx) ctx,
      retry = 0
    ),
    class = "glysmith_step"
  )
  
  ctx <- list(meta = list(logs = list(), steps = character(0)), data = list())
  bp_req <- glysmith:::new_blueprint(list(step_req))
  
  expect_message({
    ctx_result <- glysmith:::run_blueprint(bp_req, ctx)
  }, "Skipping Step .* due to missing ctx\\$data keys")
  
  expect_false("step_req" %in% ctx_result$meta$steps)
})

test_that("run_blueprint handles step failure", {
  step_fail <- structure(
    list(
      id = "step_fail",
      label = "Failing Step",
      run = function(ctx) stop("intentionally failed"),
      retry = 0
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
      retry = 0
    ),
    class = "glysmith_step"
  )
  
  ctx <- list(meta = list(logs = list(), steps = character(0)), data = list())
  bp <- glysmith:::new_blueprint(list(step_fail, step_ok))
  
  # Should not error
  expect_no_error({
    ctx_result <- glysmith:::run_blueprint(bp, ctx)
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
