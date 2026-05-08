test_that("step package dependencies are collected from a blueprint", {
  bp <- blueprint(
    step_ident_overview(),
    step_infer_structure()
  )

  pkgs <- collect_blueprint_packages(bp)

  expect_true(all(c("glyanno", "glydb", "glyrepr") %in% pkgs))
  expect_false("glyexp" %in% pkgs)
})

test_that("blueprint-scoped dependencies exclude unused glycoverse packages", {
  bp <- blueprint_default(enrich = FALSE, traits = FALSE)

  pkgs <- collect_blueprint_packages(bp)

  expect_false("glyanno" %in% pkgs)
  expect_false("glydb" %in% pkgs)
})

test_that("glyexp is a universal import, not an optional step dependency", {
  desc <- read.dcf(
    system.file("DESCRIPTION", package = "glysmith"),
    fields = c("Imports", "Suggests")
  )

  expect_match(desc[1, "Imports"], "glyexp")
  expect_no_match(desc[1, "Suggests"], "glyexp")
})

test_that("dependency install hint includes glycoverse r-universe repo", {
  hint <- format_dependency_install_hint(c("glydb", "pROC"))

  expect_true(any(grepl(
    "https://glycoverse.r-universe.dev",
    hint,
    fixed = TRUE
  )))
  expect_true(any(grepl("pak::pkg_install", hint, fixed = TRUE)))
  expect_true(any(grepl("glydb", hint, fixed = TRUE)))
  expect_true(any(grepl("pROC", hint, fixed = TRUE)))
})

test_that("dependency repos preserve existing repositories", {
  repos <- dependency_repos(c(
    CRAN = "@CRAN@",
    BioCsoft = "https://bioconductor.org/packages/release/bioc"
  ))

  expect_equal(unname(repos["glycoverse"]), "https://glycoverse.r-universe.dev")
  expect_equal(unname(repos["CRAN"]), "https://cloud.r-project.org")
  expect_equal(
    unname(repos["BioCsoft"]),
    "https://bioconductor.org/packages/release/bioc"
  )
})

test_that("dependency repos handle unnamed repository vectors", {
  expect_no_error(
    repos <- dependency_repos(c("https://cloud.r-project.org"))
  )

  expect_equal(unname(repos["glycoverse"]), "https://glycoverse.r-universe.dev")
  expect_equal(unname(repos["CRAN"]), "https://cloud.r-project.org")
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
