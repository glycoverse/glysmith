test_that("polish_report works", {
  exp <- glyexp::real_experiment2
  suppressMessages(result <- forge_analysis(exp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  polish_report(result, output_file, open = FALSE)
  expect_true(fs::file_exists(output_file))
})

test_that("polish_report works with AI polish", {
  skip_on_ci()
  skip_on_cran()
  local_mocked_bindings(
    .ask_ai = function(system_prompt, user_prompt, ...) {
    if (grepl("report section organizer", system_prompt, fixed = TRUE)) {
      return(paste(
        "## Preprocessing",
        "items: step:preprocess",
        "## DEA analysis",
        "items: step:dea_limma; plot:volcano; plot:heatmap",
        sep = "\n"
      ))
    }
    "AI response"
  },
    .ask_ai_multimodal = function(...) "AI plot description"
  )
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_api_key"))
  bp <- structure(
    list(
      preprocess = list(
        id = "preprocess",
        label = "Preprocessing",
        report = function(x) "Preprocess report."
      ),
      dea_limma = list(
        id = "dea_limma",
        label = "DEA analysis (limma)",
        report = function(x) "DEA report."
      ),
      volcano = list(
        id = "volcano",
        label = "Volcano plot",
        report = function(x) "Volcano report."
      ),
      heatmap = list(
        id = "heatmap",
        label = "Heatmap",
        report = function(x) "Heatmap report."
      )
    ),
    class = "glysmith_blueprint"
  )
  result <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(
        volcano = ggplot2::ggplot(),
        heatmap = ggplot2::ggplot()
      ),
      tables = list(),
      meta = list(
        steps = c("preprocess", "dea_limma", "volcano", "heatmap"),
        explanation = list(
          "plots$volcano" = "Volcano plot.",
          "plots$heatmap" = "Heatmap plot."
        )
      ),
      blueprint = bp
    ),
    class = "glysmith_result"
  )
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  suppressMessages(polish_report(result, output_file, open = FALSE, use_ai = TRUE))
  expect_true(fs::file_exists(output_file))
  output_lines <- readLines(output_file)
  expect_true(any(grepl("Preprocessing", output_lines, fixed = TRUE)))
  expect_true(any(grepl("DEA analysis", output_lines, fixed = TRUE)))
  expect_true(any(grepl("AI response", output_lines, fixed = TRUE)))
  expect_true(any(grepl("AI plot description", output_lines, fixed = TRUE)))
})

test_that("polish_report raises an error when API key is not set", {
  skip_on_ci()
  skip_on_cran()
  local_mocked_bindings(.ask_ai = function(...) "AI response")
  exp <- glyexp::real_experiment2
  bp <- blueprint(step_preprocess())
  withr::local_envvar(c(DEEPSEEK_API_KEY = ""))
  suppressMessages(result <- forge_analysis(exp, blueprint = bp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  expect_error(polish_report(result, output_file, open = FALSE, use_ai = TRUE))
})

test_that("polish_report captures AI errors", {
  skip_on_ci()
  skip_on_cran()
  local_mocked_bindings(
    .ask_ai = function(...) stop("AI error"),
    .ask_ai_multimodal = function(...) "AI plot description"
  )
  exp <- glyexp::real_experiment2
  bp <- blueprint(step_preprocess())
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test_api_key"))
  suppressMessages(result <- forge_analysis(exp, blueprint = bp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  warn_msgs <- character(0)
  suppressMessages(withCallingHandlers(
    polish_report(result, output_file, open = FALSE, use_ai = TRUE),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ))
  expect_true(any(grepl("AI error", warn_msgs, fixed = TRUE)))
})

test_that("polish_report omits empty step placeholders and humanizes plot titles", {
  bp <- structure(
    list(
      volcano = list(
        id = "volcano",
        label = "Volcano plot",
        report = function(x) ""
      ),
      heatmap_sig = list(
        id = "heatmap_sig",
        label = "Heatmap of significant variables",
        report = function(x) ""
      )
    ),
    class = "glysmith_blueprint"
  )
  result <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(
        volcano_H_vs_M = ggplot2::ggplot(),
        heatmap_sig = ggplot2::ggplot()
      ),
      tables = list(),
      meta = list(
        steps = c("volcano", "heatmap_sig"),
        explanation = list(
          "plots$volcano_H_vs_M" = "Volcano plot for the comparison of H_vs_M."
        )
      ),
      blueprint = bp
    ),
    class = "glysmith_result"
  )
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  suppressMessages(polish_report(result, output_file, open = FALSE))
  output_lines <- readLines(output_file)
  expect_false(any(grepl("No report content for this step.", output_lines, fixed = TRUE)))
  expect_true(any(grepl("Volcano plot: H vs M", output_lines, fixed = TRUE)))
  expect_true(any(grepl("comparison of H vs M", output_lines, fixed = TRUE)))
  expect_true(any(grepl("Heatmap of significant variables", output_lines, fixed = TRUE)))
})

test_that("parse_section_plan handles code fences and items", {
  output <- paste(
    "```",
    "## Preprocessing",
    "items: step:preprocess; plot:qc_plot",
    "",
    "## Results",
    "items: step:dea; plot:volcano",
    "```",
    sep = "\n"
  )
  plan <- glysmith:::.parse_section_plan(output)
  expect_length(plan, 2)
  expect_equal(plan[[1]]$title, "Preprocessing")
  expect_equal(plan[[1]]$items[[1]]$type, "step")
  expect_equal(plan[[1]]$items[[1]]$id, "preprocess")
  expect_equal(plan[[1]]$items[[2]]$type, "plot")
  expect_equal(plan[[1]]$items[[2]]$id, "qc_plot")
  expect_equal(plan[[2]]$title, "Results")
})

test_that("parse_section_items drops invalid tokens and respects none", {
  items <- glysmith:::.parse_section_items("step:prep; foo:bar; plot:")
  expect_length(items, 1)
  expect_equal(items[[1]]$type, "step")
  expect_equal(items[[1]]$id, "prep")
  expect_equal(glysmith:::.parse_section_items("none"), list())
})

test_that("humanize helpers format plot labels and descriptions", {
  expect_equal(
    glysmith:::.humanize_plot_label("volcano_H_vs_M"),
    "Volcano plot: H vs M"
  )
  expect_equal(
    glysmith:::.humanize_plot_label("heatmap_sig_trait"),
    "Heatmap of significant traits"
  )
  expect_equal(
    glysmith:::.humanize_plot_label("pca_sig_scores"),
    "PCA Scores (significant variables)"
  )
  expect_equal(
    glysmith:::.humanize_plot_label("tsne_sig"),
    "t-SNE (significant variables)"
  )
  expect_equal(
    glysmith:::.humanize_plot_label("umap_trait"),
    "UMAP (traits)"
  )

  desc <- "Comparison of sig_exp_vs_trait_exp and motif_exp"
  expect_equal(
    glysmith:::.humanize_plot_description(desc),
    "Comparison of significant variables vs traits and motifs"
  )
})

test_that("infer_plot_owner matches prefixes and enrich steps", {
  plot_ids <- c("pca_scores", "go", "volcano_A_vs_B", "unknown")
  step_ids <- c("pca", "sig_enrich_go", "volcano")
  owners <- glysmith:::.infer_plot_owner(plot_ids, step_ids)
  expect_equal(
    owners,
    c("pca", "sig_enrich_go", "volcano", NA_character_)
  )
})

test_that("assemble_report_sections assigns additional results", {
  step_reports <- list(
    list(id = "preprocess", label = "Preprocess", content = "prep"),
    list(id = "dea", label = "DEA", content = "dea"),
    list(id = "unused", label = "Unused", content = "unused")
  )
  plot_entries <- list(
    list(
      id = "volcano_A_vs_B",
      label = "Volcano plot: A vs B",
      description = "desc",
      plot = ggplot2::ggplot()
    )
  )
  plan <- list(
    list(
      title = "Preprocessing",
      items = list(list(type = "step", id = "preprocess"))
    ),
    list(
      title = "Analysis",
      items = list(
        list(type = "step", id = "dea"),
        list(type = "plot", id = "volcano_A_vs_B")
      )
    )
  )

  sections <- glysmith:::.assemble_report_sections(step_reports, plot_entries, plan)
  expect_equal(
    purrr::map_chr(sections, "title"),
    c("Preprocessing", "Analysis", "Additional results")
  )
  expect_equal(sections[[1]]$entries[[1]]$id, "preprocess")
  expect_equal(sections[[2]]$entries[[1]]$id, "dea")
  expect_equal(sections[[2]]$entries[[2]]$id, "volcano_A_vs_B")
  expect_equal(sections[[3]]$entries[[1]]$id, "unused")
})

test_that("build_step_reports handles missing steps and report errors", {
  bp <- structure(
    list(
      step1 = list(
        id = "step1",
        label = "Step 1",
        report = function(x) "Report <AI>remove</AI> end"
      ),
      step2 = list(
        id = "step2",
        label = "Step 2",
        report = function(x) stop("boom")
      )
    ),
    class = "glysmith_blueprint"
  )
  x <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(),
      tables = list(),
      meta = list(steps = c("step1", "step2", "missing")),
      blueprint = bp
    ),
    class = "glysmith_result"
  )

  reports <- glysmith:::.build_step_reports(x, use_ai = FALSE)
  expect_length(reports, 3)
  expect_false(grepl("remove", reports[[1]]$content, fixed = TRUE))
  expect_true(grepl("Report generation failed", reports[[2]]$content, fixed = TRUE))
  expect_true(grepl("boom", reports[[2]]$content, fixed = TRUE))
  expect_null(reports[[3]]$content)
  expect_equal(reports[[3]]$label, "missing")
})

test_that("build_step_reports uses AI polishing when enabled", {
  bp <- structure(
    list(
      step1 = list(
        id = "step1",
        label = "Step 1",
        report = function(x) "raw text"
      )
    ),
    class = "glysmith_blueprint"
  )
  x <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(),
      tables = list(),
      meta = list(steps = "step1"),
      blueprint = bp
    ),
    class = "glysmith_result"
  )

  local_mocked_bindings(
    .polish_text = function(text, api_key, model = "deepseek-chat") {
      paste0("polished: ", text)
    },
    .package = "glysmith"
  )

  reports <- glysmith:::.build_step_reports(x, use_ai = TRUE, api_key = "key")
  expect_equal(reports[[1]]$content, "polished: raw text")
})

test_that("build_plot_entries uses AI descriptions", {
  plot <- ggplot2::ggplot()
  x <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(volcano_A_vs_B = plot),
      tables = list(),
      meta = list(explanation = c("plots$volcano_A_vs_B" = "Volcano plot A_vs_B")),
      blueprint = structure(list(), class = "glysmith_blueprint")
    ),
    class = "glysmith_result"
  )

  captured <- list()
  local_mocked_bindings(
    .describe_plot_ai = function(plot, label, description, api_key, model = "deepseek-chat") {
      captured$label <<- label
      captured$description <<- description
      captured$api_key <<- api_key
      "AI desc"
    },
    .package = "glysmith"
  )

  entries <- glysmith:::.build_plot_entries(x, use_ai = TRUE, api_key = "key")
  expect_equal(entries[[1]]$description, "AI desc")
  expect_equal(captured$api_key, "key")
  expect_true(grepl("Volcano plot", entries[[1]]$label, fixed = TRUE))
})

test_that("organize_report_sections returns NULL for empty inputs", {
  expect_null(glysmith:::.organize_report_sections(list(), list(), "key"))
})

test_that("polish_report aborts when overwrite declined", {
  x <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(),
      tables = list(),
      meta = list(),
      blueprint = structure(list(), class = "glysmith_blueprint")
    ),
    class = "glysmith_result"
  )
  out_dir <- withr::local_tempdir()
  output_file <- fs::path(out_dir, "report.html")
  writeLines("existing", output_file)

  local_mocked_bindings(.ask_overwrite_file = function() "n", .package = "glysmith")
  expect_error(polish_report(x, output_file, open = FALSE), "Operation cancelled")
  expect_true(fs::file_exists(output_file))
})

test_that("polish_report overwrites output when confirmed", {
  x <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(),
      tables = list(),
      meta = list(),
      blueprint = structure(list(), class = "glysmith_blueprint")
    ),
    class = "glysmith_result"
  )
  out_dir <- withr::local_tempdir()
  output_file <- fs::path(out_dir, "report.html")
  writeLines("existing", output_file)

  local_mocked_bindings(.ask_overwrite_file = function() "y", .package = "glysmith")
  local_mocked_bindings(
    render = function(input, output_file, output_dir, params, envir, quiet) {
      path <- fs::path(output_dir, output_file)
      writeLines("rendered", path)
      path
    },
    .package = "rmarkdown"
  )

  rendered <- polish_report(x, output_file, open = FALSE)
  expect_true(fs::file_exists(rendered))
})

test_that("polish_report rejects invalid overwrite input", {
  x <- structure(
    list(
      exp = list(),
      data = list(),
      plots = list(),
      tables = list(),
      meta = list(),
      blueprint = structure(list(), class = "glysmith_blueprint")
    ),
    class = "glysmith_result"
  )
  out_dir <- withr::local_tempdir()
  output_file <- fs::path(out_dir, "report.html")
  writeLines("existing", output_file)

  local_mocked_bindings(.ask_overwrite_file = function() "maybe", .package = "glysmith")
  expect_error(polish_report(x, output_file, open = FALSE), "Invalid input")
  expect_true(fs::file_exists(output_file))
})
