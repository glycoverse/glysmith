test_that("check_glysmith_deps returns TRUE when all packages are installed", {
  skip_if_not_installed("desc")

  # All suggests packages should be installed in test environment
  expect_true(check_glysmith_deps(action = "note"))
})

test_that("check_glysmith_deps errors with action = 'error' when packages missing", {
  skip_if_not_installed("desc")

  # Mock a fake package that doesn't exist
  local_mocked_bindings(
    get_suggests_packages = function() {
      c("this_package_definitely_does_not_exist_12345")
    },
    .package = "glysmith"
  )

  expect_error(
    check_glysmith_deps(action = "error"),
    "not installed"
  )
})

test_that("check_glysmith_deps validates action parameter", {
  expect_error(
    check_glysmith_deps(action = "invalid"),
    "must be one of"
  )
})
