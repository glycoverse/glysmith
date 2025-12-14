test_that("retry=0 fails immediately", {
  fail_step <- step(
    id = "fail",
    label = "Fail Step",
    run = function(ctx) stop("Intentional failure"),
    retry = 0L
  )
  bp <- new_blueprint(list(fail_step))
  expect_error(run_blueprint(bp, list(meta = list(steps = character(0))), quiet = TRUE), "Intentional failure")
})

test_that("retry=1 succeeds if it fails once then succeeds", {
  count <- 0
  succeed_on_retry <- step(
    id = "retry_success",
    label = "Retry Success Step",
    run = function(ctx) {
      count <<- count + 1
      if (count == 1) stop("First failure")
      ctx
    },
    retry = 1L
  )
  bp <- new_blueprint(list(succeed_on_retry))
  expect_no_error(run_blueprint(bp, list(meta = list(steps = character(0))), quiet = TRUE))
  expect_equal(count, 2)
})

test_that("retry=1 fails if it fails twice", {
  count2 <- 0
  fail_on_retry <- step(
    id = "retry_fail",
    label = "Retry Fail Step",
    run = function(ctx) {
      count2 <<- count2 + 1
      stop("Always failure")
    },
    retry = 1L
  )
  bp <- new_blueprint(list(fail_on_retry))
  expect_error(run_blueprint(bp, list(meta = list(steps = character(0))), quiet = TRUE), "Always failure")
  expect_equal(count2, 2)
})
