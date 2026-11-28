test_that("nested side routes resume at their original producer", {
  bp <- blueprint(
    step_preprocess() %=>%
      br(
        "side",
        step_derive_traits() %=>%
          br("nested", step_pca()) %=>%
          step_dea_limma()
      ) %=>%
      step_dea_ttest()
  )
  nodes <- .compile_blueprint(bp)$nodes
  expect_equal(
    unname(vapply(nodes, "[[", "", "parent")),
    c("root", "node_1", "node_2", "node_2", "node_1")
  )
  expect_equal(nodes$node_3$route, "side/nested")
})
