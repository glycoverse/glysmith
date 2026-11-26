protein_step_fixture <- function(scale = "raw") {
  x <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)[1:4, 1:6]
  gp <- matrix(c(0.2, 0.5, 2, 4, 5, 7, 3, 1), 4, 6)
  dimnames(gp) <- dimnames(x)
  S4Vectors::metadata(x)$scale <- scale
  SummarizedExperiment::assay(x) <- switch(
    scale,
    raw = gp,
    log = log(gp),
    log2 = log2(gp),
    log10 = log10(gp),
    logx = gp
  )
  SummarizedExperiment::colData(x)$group <- factor(rep(c("A", "B"), each = 3))
  proteins <- unique(SummarizedExperiment::rowData(x)$protein)
  pro <- matrix(
    rep(c(0.1, 2, 3, 0.4, 8, 6), each = length(proteins)),
    length(proteins),
    6,
    dimnames = list(proteins, colnames(x))
  )
  list(exp = x, pro = pro)
}

test_that("protein scale is validated without guessing", {
  expect_identical(formals(step_adjust_protein)$pro_scale, "raw")
  for (bad in list(
    "logx",
    "unknown",
    "",
    NA_character_,
    NULL,
    c("raw", "log2")
  )) {
    expect_error(step_adjust_protein(pro_scale = bad), "pro_scale")
  }
})

test_that("protein scales match direct adjustment including records and replay", {
  skip_if_not_installed("glyclean", "0.17.0")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  for (scale in c("raw", "log", "log2", "log10")) {
    d <- protein_step_fixture(scale)
    for (pro_scale in c("raw", "log", "log2", "log10")) {
      pro <- switch(
        pro_scale,
        raw = d$pro,
        log = log(d$pro),
        log2 = log2(d$pro),
        log10 = log10(d$pro)
      )
      saveRDS(pro, path)
      for (method in c("ratio", "reg")) {
        step <- step_adjust_protein(path, method, pro_scale)
        actual <- step$run(new_ctx(d$exp, "group"))
        expected <- glyclean::adjust_protein(d$exp, pro, method, pro_scale)
        expect_identical(actual$data$exp, expected)
        expect_identical(actual$data$unadj_exp, d$exp)
        expect_identical(glyexp::get_scale(actual$data$exp), scale)
        pipeline <- glyclean::get_pipeline(actual$data$exp)
        expect_identical(tail(pipeline, 1)[[1]]$parameters$pro_scale, pro_scale)
        expect_identical(glyclean::apply_pipeline(d$exp, pipeline), expected)
      }
    }
  }
})

test_that("literal step signatures and serialized blueprints retain pro_scale", {
  skip_if_not_installed("glyclean", "0.17.0")
  d <- protein_step_fixture()
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(log2(d$pro), path)
  call <- substitute(
    step_adjust_protein(PATH, pro_scale = "log2"),
    list(PATH = path)
  )
  original <- eval(call)
  expect_match(
    paste(original$signature, collapse = " "),
    'pro_scale = "log2"',
    fixed = TRUE
  )
  replay <- eval(.parse_step_expr(original$signature))
  serialized <- unserialize(serialize(blueprint(original), NULL))
  expected <- glyclean::adjust_protein(d$exp, log2(d$pro), pro_scale = "log2")
  expect_identical(replay$run(new_ctx(d$exp, "group"))$data$exp, expected)
  actual <- suppressMessages(forge_analysis_se(d$exp, serialized))
  expect_identical(actual$data$exp, expected)
  saveRDS(d$pro, path)
  expect_identical(
    step_adjust_protein(path)$run(new_ctx(d$exp, "group"))$data$exp,
    glyclean::adjust_protein(d$exp, d$pro)
  )
})

test_that("execution retains downstream rejection of noninvertible experiment scales", {
  skip_if_not_installed("glyclean", "0.17.0")
  d <- protein_step_fixture("logx")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(d$pro, path)
  expect_error(step_adjust_protein(path)$run(new_ctx(d$exp, "group")), "logx")
})

test_that("DEA and plotting preserve the actual analysis scale", {
  skip_if_not_installed("glystats")
  skip_if_not_installed("glyvis")
  skip_if_not_installed("EnhancedVolcano")
  for (scale in c("raw", "log2", "logx")) {
    d <- protein_step_fixture(scale)
    actual <- suppressMessages(step_dea_ttest()$run(new_ctx(d$exp, "group")))
    direct <- glystats::gly_ttest(d$exp, group_col = "group")
    expect_equal(actual$data$dea_res, direct)
    expect_identical(actual$data$dea_res$preprocessing$input_scale, scale)
    expect_identical(actual$tables$dea, glystats::get_tidy_result(direct))
    if (scale == "logx") {
      expect_true(all(is.na(glystats::get_tidy_result(direct)$log2fc)))
      expect_error(step_volcano()$run(actual), "logx|fold.change|log2fc")
    } else {
      plotted <- suppressWarnings(step_volcano()$run(actual))
      expect_true("volcano" %in% names(plotted$plots))
    }
    if (scale == "log2") {
      expect_warning(
        forced <- step_dea_ttest(log_transform = TRUE)$run(
          new_ctx(d$exp, "group")
        ),
        "already|ignored|log"
      )
      expect_equal(
        glystats::get_tidy_result(forced$data$dea_res),
        glystats::get_tidy_result(direct)
      )
    }
  }
})

test_that("enrichment receives unchanged effects and propagates downstream errors", {
  skip_if_not_installed("glyfun")
  skip_if_not_installed("glystats")
  # Stop at the public enrichment boundary: no remote annotation requests.
  seen <- NULL
  local_mocked_bindings(
    enrich_ora_go = function(res, ...) {
      seen <<- res
      stop("downstream scale validation", call. = FALSE)
    },
    .package = "glyfun"
  )
  for (scale in c("raw", "log2", "logx")) {
    d <- protein_step_fixture(scale)
    ctx <- suppressMessages(step_dea_ttest()$run(new_ctx(d$exp, "group")))
    expect_error(step_sig_enrich_go()$run(ctx), "downstream scale validation")
    expect_identical(seen, ctx$data$dea_res)
  }
})

test_that("transformed protein files use the same scale for CSV TSV and RDS", {
  skip_if_not_installed("glyclean", "0.17.0")
  d <- protein_step_fixture("log10")
  pro <- log2(d$pro)
  paths <- vapply(
    c("csv", "tsv", "rds"),
    function(ext) tempfile(fileext = paste0(".", ext)),
    ""
  )
  on.exit(unlink(paths))
  tbl <- tibble::as_tibble(pro, rownames = "protein")
  readr::write_csv(tbl, paths[[1]])
  readr::write_tsv(tbl, paths[[2]])
  saveRDS(pro, paths[[3]])
  expected <- glyclean::adjust_protein(d$exp, pro, pro_scale = "log2")
  for (path in paths) {
    actual <- suppressMessages(forge_analysis_se(
      d$exp,
      blueprint(step_adjust_protein(path, pro_scale = "log2"))
    ))
    expect_equal(
      SummarizedExperiment::assay(actual$data$exp),
      SummarizedExperiment::assay(expected)
    )
    expect_identical(glyexp::get_scale(actual$data$exp), "log10")
  }
})
