test_that("branch prefixing preserves shared keys", {
  prefix <- glysmith:::.branch_prefix("br")
  keys <- c("exp", "x", "br__y")
  expect_equal(
    glysmith:::.branch_prefix_keys(keys, prefix),
    c("exp", "br__x", "br__y")
  )
})

test_that("branch overlay copies prefixed data into branch view", {
  ctx <- list(data = list("br__x" = 1, "exp" = "global", "x" = 0))
  overlay <- glysmith:::.branch_overlay_ctx(ctx, "br__")
  expect_equal(overlay$data$x, 1)
  expect_equal(overlay$data$exp, "global")
})

test_that("branch explanations are prefixed and removals are applied", {
  target <- list(
    "data$br__a" = "old",
    "plots$br__z" = "oldplot"
  )
  before <- list(
    "data$a" = "old",
    "plots$z" = "oldplot"
  )
  after <- list(
    "data$a" = "new",
    "tables$b" = "tbl"
  )
  updated <- glysmith:::.branch_update_explanations(
    target,
    before,
    after,
    "br__"
  )
  expect_equal(updated[["data$br__a"]], "new")
  expect_equal(updated[["tables$br__b"]], "tbl")
  expect_false("plots$br__z" %in% names(updated))
})

test_that("branch view result maps logs and unprefixes outputs", {
  x <- list(
    data = list("br__exp" = "exp_branch", "br__x" = 1, "y" = 2),
    plots = list("br__plot" = "p"),
    tables = list("br__tab" = "t"),
    meta = list(logs = list("br__step" = list(output = "ok")))
  )
  res <- glysmith:::.branch_view_result(x, "br", "step", "br__step")
  expect_equal(res$exp, "exp_branch")
  expect_equal(res$data$exp, "exp_branch")
  expect_equal(res$data$x, 1)
  expect_equal(res$plots$plot, "p")
  expect_equal(res$tables$tab, "t")
  expect_equal(res$meta$logs$step$output, "ok")
})

test_that("branch_to_steps rejects nested branches", {
  inner <- br("inner", step("a", "A", function(ctx) ctx))
  outer <- br("outer", inner)
  expect_error(
    glysmith:::.branch_to_steps(outer),
    "Nested branches are not supported"
  )
})


test_that("branch wrapping preserves package and repeatability metadata", {
  original <- step(
    "custom",
    "Custom step",
    function(ctx) ctx,
    packages = c("glystats", "glyvis"),
    repeatable = TRUE,
    retry = 2L
  )

  wrapped <- glysmith:::.branch_wrap_step(original, "analysis")

  expect_identical(wrapped$packages, original$packages)
  expect_identical(wrapped$repeatable, original$repeatable)
  expect_identical(wrapped$retry, original$retry)
  expect_no_error(blueprint(br("analysis", original, original)))
})

test_that("non-repeatable steps remain non-repeatable inside branches", {
  original <- step("custom", "Custom step", function(ctx) ctx)

  expect_error(
    blueprint(br("analysis", original, original)),
    "duplicated steps"
  )
})
