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

  expect_false("sample" %in% names(sample_info))
  expect_false("variable" %in% names(var_info))
  expect_identical(sample_info$group, c("control", "case"))
  expect_identical(var_info$annotation, c("first", "second", "third"))

  data <- glysmith:::.as_data_tibble(exp)
  expect_equal(nrow(data), length(SummarizedExperiment::assay(exp)))
  expect_identical(data$.sample, rep(colnames(exp), times = nrow(exp)))
  expect_identical(data$.variable, rep(rownames(exp), each = ncol(exp)))
  expect_identical(data$value, as.vector(t(SummarizedExperiment::assay(exp))))
  expect_identical(data$group, rep(c("control", "case"), times = 3))
  expect_identical(
    data$annotation,
    rep(c("first", "second", "third"), each = 2)
  )

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
  data <- glysmith:::.as_data_tibble(exp)
  expect_identical(data$sample, rep(c("s1", "s2"), times = 2))
  expect_identical(data$variable, rep(c("v1", "v2"), each = 2))
  expect_identical(data$value, c(1L, 3L, 2L, 4L))
})

test_that("long-form conversion handles unique and empty dimensions", {
  exp <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = matrix(seq_len(4), nrow = 2)),
    rowData = S4Vectors::DataFrame(annotation = c("first", "second")),
    colData = S4Vectors::DataFrame(group = c("control", "case"))
  )
  rownames(exp) <- c("v1", "v2")
  colnames(exp) <- c("s1", "s2")
  data <- glysmith:::.as_data_tibble(exp)
  expect_identical(
    names(data),
    c(".sample", "group", ".variable", "annotation", "value")
  )
  expect_identical(data$value, c(1L, 3L, 2L, 4L))
  expect_identical(data$group, rep(c("control", "case"), times = 2))
  expect_identical(data$annotation, rep(c("first", "second"), each = 2))
  expect_equal(nrow(glysmith:::.as_data_tibble(exp[FALSE, ])), 0)
  expect_equal(nrow(glysmith:::.as_data_tibble(exp[, FALSE])), 0)
})

test_that("SE annotation round trips never rename axes or discard annotations", {
  exp <- SummarizedExperiment::SummarizedExperiment(
    assays = list(
      abundance = matrix(
        1:6,
        3,
        dimnames = list(
          c("v1", "v2", "v3"),
          c("s1", "s2")
        )
      )
    ),
    rowData = S4Vectors::DataFrame(variable = c("same", "same", "")),
    colData = S4Vectors::DataFrame(sample = c("", ""), group = c("A", "B"))
  )
  restored <- glysmith:::.set_sample_info(exp, glysmith:::.get_sample_info(exp))
  restored <- glysmith:::.set_var_info(restored, glysmith:::.get_var_info(exp))
  expect_identical(restored, exp)
  filtered <- glysmith:::.filter_groups(exp, "B")
  expect_identical(colnames(filtered), "s2")
  expect_identical(SummarizedExperiment::colData(filtered)$sample, "")
  data <- glysmith:::.as_data_tibble(exp)
  expect_identical(data$.sample, rep(colnames(exp), 3))
  expect_identical(data$.variable, rep(rownames(exp), each = 2))
  expect_identical(data$sample, rep("", 6))
  expect_identical(data$variable, rep(c("same", "same", ""), each = 2))
})

test_that("empty blueprints preserve ordinary sample and variable annotations", {
  for (exp in test_glysmith_containers()[c("glycomic", "glycoproteomic")]) {
    exp <- exp[1:3, 1:4]
    SummarizedExperiment::colData(exp)$sample <- c("", "other", "other", NA)
    SummarizedExperiment::rowData(exp)$variable <- c("other", "other", "")
    before <- exp
    result <- forge_analysis(exp, blueprint())
    expect_identical(dimnames(result$exp), dimnames(before))
    expect_identical(
      SummarizedExperiment::assay(result$exp, withDimnames = FALSE),
      SummarizedExperiment::assay(before, withDimnames = FALSE)
    )
    expect_identical(
      SummarizedExperiment::colData(result$exp)$sample,
      SummarizedExperiment::colData(before)$sample
    )
    expect_identical(
      SummarizedExperiment::rowData(result$exp),
      SummarizedExperiment::rowData(before)
    )
    result <- forge_analysis(exp, blueprint(), group_col = "sample")
    expect_identical(colnames(result$exp), colnames(before))
    expect_identical(
      SummarizedExperiment::colData(result$exp)$sample,
      SummarizedExperiment::colData(before)$sample
    )
  }
})

test_that("reserved output names do not replace SE annotation values", {
  exp <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = matrix(1:4, 2)),
    rowData = S4Vectors::DataFrame(.variable = c("a", "b"), value = 5:6),
    colData = S4Vectors::DataFrame(.sample = c("c", "d"), value = 7:8)
  )
  data <- glysmith:::.as_data_tibble(exp)
  expect_identical(data$.sample, rep(NA_character_, 4))
  expect_identical(data$.variable, rep(NA_character_, 4))
  expect_identical(data$.sample.1, rep(c("c", "d"), 2))
  expect_identical(data$.variable.1, rep(c("a", "b"), each = 2))
  expect_identical(data$value, c(1L, 3L, 2L, 4L))
  restored <- glysmith:::.set_sample_info(exp, glysmith:::.get_sample_info(exp))
  restored <- glysmith:::.set_var_info(restored, glysmith:::.get_var_info(exp))
  expect_identical(restored, exp)
})
