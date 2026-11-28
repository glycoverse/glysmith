coda_input <- function(n = 7L) {
  exp <- glyexp::real_experiment2[seq_len(n), seq_len(12)]
  group <- factor(rep(c("A", "B"), each = 6))
  SummarizedExperiment::colData(exp)$group <- group

  # One stable ALR reference and opposite group effects in the other rows.
  values <- withr::with_seed(71, matrix(stats::rnorm(n * 12, sd = 0.22), n))
  values[1, ] <- 0
  values[-1, group == "B"] <- values[-1, group == "B"] +
    rep_len(c(1, -1), n - 1)
  dimnames(values) <- dimnames(exp)
  SummarizedExperiment::assay(exp) <- 8 * 2^values
  exp
}

test_that("step_coda dispatches each method and forwards its parameters", {
  skip_if_not_installed("glyclean")
  expect_identical(formals(step_coda)$method, "auto")
  expect_error(step_coda("unknown"), "method")

  for (n in c(7L, 51L)) {
    exp <- coda_input(n)
    ctx <- new_ctx(exp, "group")
    for (method in c("auto", "clr", "alr")) {
      transform <- switch(
        method,
        auto = glyclean::auto_coda,
        clr = glyclean::transform_clr,
        alr = glyclean::transform_alr
      )
      expected <- suppressMessages(transform(
        exp,
        by = "group",
        gamma = 0.2,
        group_scales = c(1, 1.2),
        seed = 47
      ))
      s <- step_coda(
        method,
        by = "group",
        gamma = 0.2,
        group_scales = c(1, 1.2),
        seed = 47
      )
      actual <- suppressMessages(s$run(ctx))
      expect_identical(actual$data$pre_coda_exp, exp)
      expect_identical(actual$data$exp, expected)
      expect_identical(glyexp::get_scale(actual$data$exp), "logx")
      uses_alr <- method == "alr" || (method == "auto" && n > 50L)
      expect_equal(nrow(actual$data$exp), n - as.integer(uses_alr))
    }
  }
})

test_that("PCA and DEA analyze CoDA coordinates and preserve native effects", {
  exp <- coda_input()
  bp <- blueprint(
    step_coda(gamma = 0) %=>%
      fork(
        step_pca(),
        blueprint(
          step_dea_limma(filter_fc_cutoff = 100) %=>%
            fork(step_volcano(), step_filter_sig_vars() %=>% step_heatmap())
        )
      )
  )
  res <- suppressMessages(forge_analysis_se(exp, bp))
  expect_identical(res$meta$steps, paste0("node_", c(1, 2, 3, 5, 6)))
  expect_identical(res$meta$nodes$node_4$status, "skipped")

  values <- log2(SummarizedExperiment::assay(exp))
  clr <- sweep(values, 2, colMeans(values))
  expect_equal(SummarizedExperiment::assay(res$exp), clr)
  pca <- glystats::gly_pca(res$exp, log_transform = FALSE)
  expect_equal(
    res$tables$node_2__pca_samples,
    glystats::get_tidy_result(pca, "samples")
  )

  fit <- res$data$node_3
  expect_identical(fit$preprocessing$analysis_scale, "logx")
  expect_false(fit$preprocessing$log_transform)
  in_b <- SummarizedExperiment::colData(exp)$group == "B"
  effects <- rowMeans(clr[, in_b]) - rowMeans(clr[, !in_b])
  tbl <- res$tables$node_3__dea
  ids <- tbl[[.stats_id_column(fit)]]
  expect_equal(tbl$estimate, unname(effects[ids]))
  expect_true(all(is.na(tbl$log2fc)))
  expect_true(any(tbl$p_adj < 0.05))
  expect_setequal(rownames(res$data$node_5), ids[tbl$p_adj < 0.05])
  expect_identical(glyexp::get_scale(res$data$node_5), "logx")
})

test_that("ALR differential tests only use the retained coordinates", {
  exp <- coda_input(51)
  bp <- blueprint(step_coda(by = "group", gamma = 0) %=>% step_dea_ttest())
  res <- suppressMessages(forge_analysis_se(exp, bp))
  expect_identical(rownames(res$exp), rownames(exp)[-1])
  expect_setequal(
    res$tables$node_2__dea[[.stats_id_column(res$data$node_2)]],
    rownames(res$exp)
  )
  expect_equal(
    res$tables$node_2__dea,
    glystats::get_tidy_result(suppressMessages(glystats::gly_ttest(res$exp)))
  )
})

test_that("a motif branch transforms quantified motifs independently", {
  exp <- coda_input()
  bp <- blueprint(
    step_quantify_branch_motifs() %=>%
      br(
        "motifs",
        step_coda("clr", gamma = 0) %=>%
          step_dea_limma()
      )
  )
  res <- suppressMessages(forge_analysis_se(exp, bp))
  input <- res$data$node_1
  expect_identical(res$data$node_2__pre_coda_exp, input)
  expect_equal(
    res$data$node_2,
    glyclean::transform_clr(input, gamma = 0)
  )
  expect_identical(
    res$data$node_3$preprocessing$analysis_scale,
    "logx"
  )
  expect_identical(res$exp, input)
  expect_identical(S4Vectors::metadata(res$exp)$scale, "raw")
})

test_that("site-wise CoDA runs downstream tests and skips fold-change consumers", {
  skip_if_not_installed("glyclean")
  skip_if_not_installed("glystats")
  exp <- glyexp::real_experiment[1:4, ]
  input <- coda_input(5)[-1, ]
  SummarizedExperiment::colData(exp)$group <- SummarizedExperiment::colData(
    input
  )$group
  SummarizedExperiment::rowData(exp)$protein <- rep("P1", 4)
  SummarizedExperiment::rowData(exp)$protein_site <- c(12L, 12L, 24L, 24L)
  values <- SummarizedExperiment::assay(input)
  dimnames(values) <- dimnames(exp)
  SummarizedExperiment::assay(exp) <- log2(values)
  exp <- glyexp::set_scale(exp, "log2")

  ctx <- suppressMessages(step_dea_ttest()$run(new_ctx(exp, "group")))
  ctx <- suppressMessages(step_coda(gamma = 0)$run(ctx))
  expect_equal(ctx$data$exp, glyclean::transform_clr(exp, gamma = 0))
  expect_true(step_volcano()$condition(ctx)$check)
  expect_true(step_sig_enrich_go()$condition(ctx)$check)
  ctx <- suppressMessages(step_dea_ttest()$run(ctx))
  expect_false(step_volcano()$condition(ctx)$check)
  expect_false(step_sig_enrich_go()$condition(ctx)$check)
  expect_false(step_derive_traits()$condition(ctx)$check)
  expect_false(step_quantify_dynamic_motifs()$condition(ctx)$check)
  expect_false(step_quantify_branch_motifs()$condition(ctx)$check)
  expect_error(step_coda("alr")$run(new_ctx(exp, "group")), "glycoproteomics")
})

test_that("a failed CoDA step stops downstream tests on the original scale", {
  exp <- coda_input()
  SummarizedExperiment::assay(exp)[1, 1] <- 0
  bp <- blueprint(step_coda() %=>% step_dea_limma())
  x <- suppressMessages(forge_analysis_se(exp, bp))
  expect_match(x$meta$nodes$node_1$reason, "Zero values")
  expect_identical(x$meta$nodes$node_2$status, "blocked")
  bp <- blueprint(br("ratios", step_coda() %=>% step_dea_limma()))
  x <- suppressMessages(forge_analysis_se(exp, bp))
  expect_match(x$meta$nodes$node_1$reason, "Zero values")
  expect_identical(x$meta$nodes$node_2$status, "blocked")
})

test_that("legacy CoDA input and output retain their container and scale", {
  skip_if_not_installed("glyclean")
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  exp <- glyexp::from_se(coda_input(), exp_type = "glycomics")
  res <- suppressMessages(forge_analysis(
    exp,
    blueprint(step_coda("clr", gamma = 0))
  ))
  expect_s3_class(res$exp, "glyexp_experiment")
  expect_s3_class(res$data$node_1__pre_coda_exp, "glyexp_experiment")
  expect_identical(res$exp$meta_data$scale, "logx")
  expect_equal(res$data$node_1__pre_coda_exp$expr_mat, exp$expr_mat)
})
