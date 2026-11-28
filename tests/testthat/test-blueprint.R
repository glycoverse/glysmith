test_that("saved trees round-trip and flat RDS plans are rejected", {
  bp <- blueprint(step_dea_limma() %=>% step_volcano())
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

test_that("commas start independent entries and only the operator connects them", {
  parallel <- blueprint(first = step_zero_to_na(), second = step_pca(), )
  nodes <- .compile_blueprint(parallel)$nodes
  expect_identical(
    vapply(nodes, "[[", "", "parent") |> unname(),
    c("root", "root")
  )
  expect_identical(
    vapply(nodes, "[[", "", "route") |> unname(),
    c("first", "second")
  )
  expect_identical(.compile_blueprint(parallel)$main_exp, "root")
  serial <- blueprint(step_zero_to_na() %=>% step_pca(), )
  expect_identical(.compile_blueprint(serial)$nodes$node_2$parent, "node_1")
  expect_identical(.compile_blueprint(serial)$main_exp, "node_1")

  unary <- blueprint(step_dea_limma() %=>% fork(step_volcano()))
  expect_identical(.compile_blueprint(unary)$nodes$node_2$parent, "node_1")
  expect_error(
    blueprint(step_dea_limma(), step_volcano()),
    "requires.*contrast"
  )
  expect_error(blueprint(step_pca(), , step_heatmap()), "empty")
  expect_error(fork(step_pca(), , step_heatmap()), "empty")
  expect_error(fork(), "one or more")
  expect_error(parallel %=>% step_heatmap(), "terminal step or fork")
  expect_error(unary %=>% step_dea_limma(), "terminal step or fork")
  expect_error(br("side", step_zero_to_na(), step_pca()), "unused argument")
  expect_error(step_pca() %=>% 42, "operands")
})

test_that("RDS with the earlier comma-sequence format is rejected", {
  old <- structure(
    list(step_pca(), step_heatmap()),
    class = "glysmith_blueprint",
    format_version = 2L
  )
  file <- withr::local_tempfile()
  saveRDS(old, file)
  expect_error(read_blueprint(file), "Incompatible blueprint")
  expect_error(blueprint(step_preprocess() %=>% old), "Incompatible blueprint")
})
