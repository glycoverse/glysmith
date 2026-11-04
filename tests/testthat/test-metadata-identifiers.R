test_that("metadata extraction preserves repeated SE identifiers", {
  exp <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = matrix(seq_len(6), nrow = 3)),
    rowData = S4Vectors::DataFrame(annotation = c("first", "second", "third")),
    colData = S4Vectors::DataFrame(group = c("control", "case"))
  )
  rownames(exp) <- c("feature", "feature", "feature.1")
  colnames(exp) <- c("sample", "sample")

  sample_info <- glysmith:::.get_sample_info(exp)
  var_info <- glysmith:::.get_var_info(exp)

  expect_identical(sample_info$sample, colnames(exp))
  expect_identical(var_info$variable, rownames(exp))
  expect_identical(sample_info$group, c("control", "case"))
  expect_identical(var_info$annotation, c("first", "second", "third"))

  restored <- glysmith:::.set_sample_info(exp, sample_info)
  restored <- glysmith:::.set_var_info(restored, var_info)
  expect_identical(dimnames(restored), dimnames(exp))
  expect_identical(
    SummarizedExperiment::assay(restored),
    SummarizedExperiment::assay(exp)
  )
})

test_that("metadata extraction retains supplied identifier columns", {
  exp <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = matrix(seq_len(4), nrow = 2)),
    rowData = S4Vectors::DataFrame(variable = c("v1", "v2")),
    colData = S4Vectors::DataFrame(sample = c("s1", "s2"))
  )
  rownames(exp) <- c("feature", "feature")
  colnames(exp) <- c("sample", "sample")

  expect_identical(glysmith:::.get_sample_info(exp)$sample, c("s1", "s2"))
  expect_identical(glysmith:::.get_var_info(exp)$variable, c("v1", "v2"))
})
