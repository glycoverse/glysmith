test_that("run_blueprint captures nested warnings, output and messages per node", {
  noisy <- function() {
    warning("first")
    warning("second")
    message("message")
    cat("output")
  }
  s <- step("noisy", "Noisy", function(ctx) {
    noisy()
    ctx
  })
  x <- run_blueprint(
    blueprint(fork(s, s)),
    new_ctx(glyexp::real_experiment2, "group"),
    quiet = TRUE
  )
  expect_named(x$meta$logs, c("node_1", "node_2"))
  expect_identical(x$meta$logs$node_1$output, "output")
  expect_identical(x$meta$logs$node_2$message, "message")
  expect_identical(
    vapply(x$meta$logs$node_2$warning, conditionMessage, ""),
    c("first", "second")
  )
})
