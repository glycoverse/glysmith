test_that("saved trees round-trip and flat RDS plans are rejected", {
  bp <- blueprint(step_dea_limma(), step_volcano())
  path <- withr::local_tempfile()
  write_blueprint(bp, path)
  expect_equal(read_blueprint(path), bp)
  old <- structure(list(step_pca()), class = "glysmith_blueprint")
  saveRDS(old, path)
  expect_error(read_blueprint(path), "Incompatible blueprint")
  expect_error(
    forge_analysis(glyexp::real_experiment2, old),
    "Incompatible blueprint"
  )
})
