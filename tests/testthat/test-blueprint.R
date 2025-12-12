test_that("blueprint checks ctx$data dependencies", {
  expect_invisible(
    blueprint(list(
      step(id = "a", label = "A", run = function(ctx) ctx, generate = c("x", "y")),
      step(id = "b", label = "B", run = function(ctx) ctx, require = "x", generate = "z"),
      step(id = "c", label = "C", run = function(ctx) ctx, require = c("y", "z"))
    ))
  )

  expect_error(
    blueprint(list(step(id = "b", label = "B", run = function(ctx) ctx, require = "x"))),
    "requires missing ctx\\$data keys", fixed = FALSE
  )
})
