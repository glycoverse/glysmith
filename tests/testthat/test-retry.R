test_that("each node retries only its own failed attempts", {
  for (n in 0:2) {
    count <- 0L
    s <- step(
      "try",
      "Try",
      function(ctx) {
        count <<- count + 1L
        if (count < 3L) {
          stop("retry me")
        }
        ctx
      },
      retry = n
    )
    x <- run_blueprint(
      blueprint(s),
      new_ctx(glyexp::real_experiment2, "group"),
      quiet = TRUE
    )
    expect_equal(count, n + 1L)
    expect_identical(
      x$meta$nodes$node_1$status,
      if (n == 2L) "success" else "failed"
    )
    if (n < 2L) expect_identical(x$meta$nodes$node_1$reason, "retry me")
  }
})
