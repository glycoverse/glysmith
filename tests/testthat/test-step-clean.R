clean_input <- function() {
  exp <- glyexp::real_experiment2[1:52, 1:12]
  mat <- withr::with_seed(
    127,
    matrix(
      stats::rlnorm(52 * 12, meanlog = seq(2, 5, length.out = 52), sdlog = 0.4),
      nrow = 52
    )
  )
  dimnames(mat) <- dimnames(exp)
  SummarizedExperiment::assay(exp) <- mat
  SummarizedExperiment::colData(exp)$group <- factor(rep(c("A", "B"), each = 6))
  SummarizedExperiment::colData(exp)$plate <- rep(c("P1", "P2"), 6)
  exp
}

test_that("every glyclean normalization, imputation and filtering method is selectable", {
  skip_if_not_installed("glyclean")
  exports <- getNamespaceExports("glyclean")
  families <- list(
    normalize = step_normalize,
    impute = step_impute,
    remove = step_remove
  )
  for (family in names(families)) {
    constructor <- families[[family]]
    prefix <- paste0(family, "_")
    methods <- exports[startsWith(exports, prefix)]
    for (operation in methods) {
      selected <- constructor(substring(operation, nchar(prefix) + 1L))
      expect_identical(selected$id, operation)
    }
    expect_identical(constructor()$id, paste0("auto_", family))
    expect_error(constructor("unknown"), class = "rlang_error")
  }
  expect_error(step_impute("bpca", dreamai_methods = "KNN"), "dreamai")
})

test_that("custom preprocessing composes in order and retains the raw data", {
  skip_if_not_installed("glyclean")
  exp <- clean_input()
  SummarizedExperiment::assay(exp)[1, 1:11] <- 0
  SummarizedExperiment::assay(exp)[2, 2] <- NA_real_
  bp <- blueprint(
    step_zero_to_na(),
    step_remove("rare", prop = 0.5, by = "group"),
    step_impute("min_prob", by = "group", q = 0.03, seed = 73),
    step_normalize("median_quotient", by = "plate"),
    step_remove("constant"),
    step_transform_log(base = 10)
  )
  result <- suppressMessages(forge_analysis(exp, bp))
  expected <- suppressMessages(
    exp |>
      glyclean::zero_to_na() |>
      glyclean::remove_rare(prop = 0.5, by = "group") |>
      glyclean::impute_min_prob(by = "group", q = 0.03, seed = 73) |>
      glyclean::normalize_median_quotient(by = "plate") |>
      glyclean::remove_constant() |>
      glyclean::transform_log(base = 10)
  )
  expect_identical(result$exp, expected)
  expect_identical(result$data$raw_exp, exp)
  expect_identical(result$meta$steps, names(bp))
})

test_that("automatic operations and manual batch correction use the selected arguments", {
  skip_if_not_installed("glyclean")
  exp <- clean_input()
  SummarizedExperiment::assay(exp)[1, 1] <- NA_real_
  ctx <- new_ctx(exp, "group")
  steps <- list(
    step_remove(preset = "discovery", group_col = "group"),
    step_normalize(group_col = NULL),
    step_impute(seed = 420),
    step_aggregate(
      to_level = "g",
      f = mean,
      standardize_variable = FALSE,
      seed = 37
    ),
    step_correct_batch_effect(batch_col = "plate", prop_threshold = 1),
    step_correct_batch_effect("limma", batch = "plate", group = "group"),
    step_correct_batch_effect("combat", batch = "plate", group = "group")
  )
  expected <- suppressMessages(list(
    glyclean::auto_remove(exp, preset = "discovery", group_col = "group"),
    glyclean::auto_normalize(exp, group_col = NULL),
    glyclean::auto_impute(exp, seed = 420),
    glyclean::aggregate(
      exp,
      to_level = "g",
      f = mean,
      standardize_variable = FALSE,
      seed = 37
    ),
    glyclean::auto_correct_batch_effect(
      exp,
      batch_col = "plate",
      prop_threshold = 1
    ),
    glyclean::correct_batch_effect(
      exp,
      method = "limma",
      batch = "plate",
      group = "group"
    ),
    glyclean::correct_batch_effect(
      exp,
      method = "combat",
      batch = "plate",
      group = "group"
    )
  ))
  for (i in seq_along(steps)) {
    expect_identical(
      suppressMessages(steps[[i]]$run(ctx))$data$exp,
      expected[[i]]
    )
  }
  batch <- suppressMessages(step_detect_batch_effect("plate", "group")$run(ctx))
  p <- suppressMessages(glyclean::detect_batch_effect(
    exp,
    batch = "plate",
    group = "group"
  ))
  expect_identical(
    batch$tables$batch_effect,
    tibble::tibble(.variable = rownames(exp), p_value = unname(p))
  )
  expect_identical(batch$data, ctx$data)
})

test_that("saved blueprints retain preprocessing parameters inside branches", {
  skip_if_not_installed("glyclean")
  exp <- clean_input()
  base <- 10
  bp <- blueprint(
    br("logged", step_transform_log(base = base), step_remove("constant"))
  )
  base <- 2
  file <- withr::local_tempfile()
  write_blueprint(bp, file)
  result <- suppressMessages(forge_analysis(exp, read_blueprint(file)))
  expect_identical(result$exp, exp)
  expect_identical(result$data$logged__raw_exp, exp)
  expect_identical(
    result$data$logged__exp,
    suppressMessages(glyclean::remove_constant(glyclean::transform_log(
      exp,
      base = 10
    )))
  )
})

test_that("granular and automatic preprocessing share the first input", {
  skip_if_not_installed("glyclean")
  exp <- clean_input()
  res <- suppressMessages(forge_analysis(
    exp,
    blueprint(
      step_normalize("median"),
      step_preprocess(standardize_variable = FALSE, seed = 19),
      step_remove("constant"),
      step_remove("constant")
    )
  ))
  expect_identical(res$data$raw_exp, exp)
  expect_identical(
    res$exp,
    suppressMessages(
      exp |>
        glyclean::normalize_median() |>
        glyclean::auto_clean(standardize_variable = FALSE, seed = 19) |>
        glyclean::remove_constant() |>
        glyclean::remove_constant()
    )
  )
})

test_that("invalid preprocessing stops dependent analysis and legacy inputs keep their class", {
  skip_if_not_installed("glyclean")
  exp <- clean_input()
  invalid <- blueprint(br(
    "bad",
    step_transform_log(base = 3),
    step_normalize()
  ))
  expect_error(suppressMessages(forge_analysis(exp, invalid)), "base")
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  legacy <- glyexp::from_se(exp, exp_type = "glycomics")
  result <- suppressMessages(forge_analysis(
    legacy,
    blueprint(step_transform_log())
  ))
  expect_s3_class(result$exp, "glyexp_experiment")
  expect_s3_class(result$data$raw_exp, "glyexp_experiment")
  expect_identical(result$data$raw_exp$expr_mat, legacy$expr_mat)
  expect_equal(result$exp$expr_mat, log2(legacy$expr_mat))
  expect_identical(result$exp$meta_data$scale, "log2")
})

test_that("a selected QC plot uses its arguments and its requested stage", {
  skip_if_not_installed("glyclean")
  operations <- grep("^plot_", getNamespaceExports("glyclean"), value = TRUE)
  for (operation in operations) {
    expect_s3_class(
      step_plot_qc(method = sub("^plot_", "", operation)),
      "glysmith_step"
    )
  }
  exp <- clean_input()
  bp <- blueprint(
    step_plot_qc("pre", method = "rle", by = "plate"),
    step_plot_qc(
      "post",
      method = "missing_bar",
      on = "variables",
      plot_width = 9
    )
  )
  result <- suppressMessages(forge_analysis(exp, bp))
  expect_named(result$plots, c("qc_pre_rle", "qc_missing_variables_bar"))
  expect_identical(result$plots$qc_missing_variables_bar$width, 9)
  expect_equal(
    cast_plot(result, "qc_pre_rle")$data,
    glyclean::plot_rle(exp, by = "plate")$data
  )
  expect_equal(
    cast_plot(result, "qc_missing_variables_bar")$data,
    glyclean::plot_missing_bar(exp, on = "variables")$data
  )
  expect_error(step_plot_qc(method = "unknown"), class = "rlang_error")
})

test_that("DreamAI forwards component selection on glyclean versions that provide it", {
  skip_if_not_installed("glyclean")
  exp <- clean_input()[1:12, 1:6]
  SummarizedExperiment::assay(exp)[2, 3] <- NA_real_
  s <- step_impute(
    "dreamai",
    dreamai_methods = "SpectroFM",
    iter_SpectroFM = 2,
    seed = 73
  )
  ctx <- new_ctx(exp, "group")
  if (!"impute_dreamai" %in% getNamespaceExports("glyclean")) {
    expect_error(s$run(ctx), "impute_dreamai")
  } else {
    skip_if_not_installed("callr")
    impute <- getExportedValue("glyclean", "impute_dreamai")
    expect_identical(
      s$run(ctx)$data$exp,
      impute(exp, method = "SpectroFM", iter_SpectroFM = 2, seed = 73)
    )
  }
})
