test_that("step package dependencies are collected from a blueprint", {
  bp <- blueprint(
    step_ident_overview(),
    step_infer_structure()
  )

  pkgs <- collect_blueprint_packages(bp)

  expect_true(all(c("glyexp", "glyanno", "glydb", "glyrepr") %in% pkgs))
})

test_that("blueprint-scoped dependencies exclude unused glycoverse packages", {
  bp <- blueprint_default(enrich = FALSE, traits = FALSE)

  pkgs <- collect_blueprint_packages(bp)

  expect_false("glyanno" %in% pkgs)
  expect_false("glydb" %in% pkgs)
})

test_that("check_glysmith_deps uses only blueprint packages", {
  bp <- blueprint(
    step(
      "custom",
      "Custom step",
      function(ctx) ctx,
      packages = "this_package_definitely_does_not_exist_12345"
    )
  )

  expect_error(
    check_glysmith_deps(blueprint = bp, action = "error"),
    "not installed"
  )
})

test_that("check_glysmith_deps validates blueprint input", {
  expect_error(
    check_glysmith_deps(blueprint = list(), action = "note"),
    "glysmith_blueprint"
  )
})

test_that("check_glysmith_deps validates action parameter", {
  expect_error(
    check_glysmith_deps(action = "invalid"),
    "must be one of"
  )
})
