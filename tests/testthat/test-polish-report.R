test_that("polish_report works", {
  exp <- glyexp::real_experiment2
  suppressMessages(result <- forge_analysis(exp))
  tmp_dir <- withr::local_tempdir()
  output_file <- fs::path(tmp_dir, "polish_report.html")
  suppressMessages(polish_report(result, output_file, open = FALSE))
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
  expect_error(
    suppressMessages(polish_report(result, output_file, open = FALSE, use_ai = TRUE))
  )
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

  suppressMessages(
    reports <- glysmith:::.build_step_reports(x, use_ai = TRUE, api_key = "key")
  )
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
    .describe_plot_ai = function(plot, label, description, api_key, width = NULL, height = NULL, model = "deepseek-chat") {
      captured$label <<- label
      captured$description <<- description
      captured$api_key <<- api_key
      "AI desc"
    },
    .package = "glysmith"
  )

  suppressMessages(
    entries <- glysmith:::.build_plot_entries(x, use_ai = TRUE, api_key = "key")
  )
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
  expect_error(
    suppressMessages(polish_report(x, output_file, open = FALSE)),
    "Operation cancelled"
  )
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

  suppressMessages(rendered <- polish_report(x, output_file, open = FALSE))
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
  expect_error(
    suppressMessages(polish_report(x, output_file, open = FALSE)),
    "Invalid input"
  )
  expect_true(fs::file_exists(output_file))
})

# Tests for .polish_text
test_that(".polish_text returns original text for NULL or empty input", {
  expect_null(glysmith:::.polish_text(NULL, "key"))
  expect_equal(glysmith:::.polish_text("", "key"), "")
})

test_that(".polish_text calls ask_ai with correct prompts", {
  captured <- list()
  local_mocked_bindings(
    .ask_ai = function(system_prompt, user_prompt, api_key, model) {
      captured$system_prompt <<- system_prompt
      captured$user_prompt <<- user_prompt
      "polished text"
    },
    .package = "glysmith"
  )

  result <- glysmith:::.polish_text("original text", "key")
  expect_equal(result, "polished text")
  expect_true(grepl("scientific writing assistant", captured$system_prompt))
  expect_equal(captured$user_prompt, "original text")
})

test_that(".polish_text returns original text on AI error", {
  local_mocked_bindings(
    .ask_ai = function(...) stop("AI failed"),
    .package = "glysmith"
  )

  expect_warning(result <- glysmith:::.polish_text("original text", "key"))
  expect_equal(result, "original text")
})

# Tests for .humanize_on_label
test_that(".humanize_on_label maps experiment types correctly", {
  expect_equal(glysmith:::.humanize_on_label("sig_exp"), "significant variables")
  expect_equal(glysmith:::.humanize_on_label("trait_exp"), "traits")
  expect_equal(glysmith:::.humanize_on_label("sig_trait_exp"), "significant traits")
  expect_equal(glysmith:::.humanize_on_label("motif_exp"), "motifs")
  expect_equal(glysmith:::.humanize_on_label("sig_motif_exp"), "significant motifs")
  expect_equal(glysmith:::.humanize_on_label("exp"), "variables")
})

# Tests for .on_suffix_labels
test_that(".on_suffix_labels returns expected labels", {
  labels <- glysmith:::.on_suffix_labels()
  expect_equal(labels[["sig"]], "significant variables")
  expect_equal(labels[["trait"]], "traits")
  expect_equal(labels[["sig_trait"]], "significant traits")
  expect_equal(labels[["motif"]], "motifs")
  expect_equal(labels[["sig_motif"]], "significant motifs")
})

# Tests for .extract_on_suffix
test_that(".extract_on_suffix extracts suffix and rest correctly", {
  result <- glysmith:::.extract_on_suffix(c("sig", "exp"))
  expect_equal(result$suffix, "sig")
  expect_equal(result$rest, "exp")

  result <- glysmith:::.extract_on_suffix(c("sig_trait", "exp"))
  expect_equal(result$suffix, "sig_trait")
  expect_equal(result$rest, "exp")

  result <- glysmith:::.extract_on_suffix(c("scores"))
  expect_null(result$suffix)
  expect_equal(result$rest, "scores")

  result <- glysmith:::.extract_on_suffix(character(0))
  expect_null(result$suffix)
  expect_equal(result$rest, character(0))
})

# Tests for .fix_plot_abbrev
test_that(".fix_plot_abbrev fixes common abbreviations", {
  expect_equal(glysmith:::.fix_plot_abbrev("Pca Plot"), "PCA Plot")
  expect_equal(glysmith:::.fix_plot_abbrev("Umap Analysis"), "UMAP Analysis")
  expect_equal(glysmith:::.fix_plot_abbrev("Tsne Visualization"), "t-SNE Visualization")
  expect_equal(glysmith:::.fix_plot_abbrev("Dea Results"), "DEA Results")
  expect_equal(glysmith:::.fix_plot_abbrev("Go Enrichment"), "GO Enrichment")
  expect_equal(glysmith:::.fix_plot_abbrev("Kegg Pathway"), "KEGG Pathway")
  expect_equal(glysmith:::.fix_plot_abbrev("Sig Genes"), "Significant Genes")
  expect_equal(glysmith:::.fix_plot_abbrev("A vs B"), "A vs B")  # Vs already replaced
})

# Tests for .parse_section_items edge cases
test_that("parse_section_items handles semicolon-separated items", {
  items <- glysmith:::.parse_section_items("step:a; step:b; plot:c")
  expect_length(items, 3)
  expect_equal(items[[1]]$type, "step")
  expect_equal(items[[1]]$id, "a")
  expect_equal(items[[2]]$id, "b")
  expect_equal(items[[3]]$type, "plot")
  expect_equal(items[[3]]$id, "c")
})

test_that("parse_section_items trims whitespace", {
  items <- glysmith:::.parse_section_items("  step:a  ;  plot:b  ")
  expect_equal(items[[1]]$id, "a")
  expect_equal(items[[2]]$id, "b")
})

test_that("parse_section_items handles na and n/a", {
  expect_equal(glysmith:::.parse_section_items("n/a"), list())
  expect_equal(glysmith:::.parse_section_items("NA"), list())
  expect_equal(glysmith:::.parse_section_items("none"), list())
})

# Tests for .parse_section_plan edge cases
test_that("parse_section_plan handles empty output", {
  expect_null(glysmith:::.parse_section_plan(""))
  expect_null(glysmith:::.parse_section_plan(NULL))
})

test_that("parse_section_plan handles output without items line", {
  output <- "## Section Title\nSome other content"
  plan <- glysmith:::.parse_section_plan(output)
  expect_length(plan, 1)
  expect_equal(plan[[1]]$title, "Section Title")
  expect_equal(plan[[1]]$items, list())
})

test_that("parse_section_plan handles multiple sections", {
  output <- paste(
    "## First Section",
    "items: step:a; plot:b",
    "",
    "## Second Section",
    "items: step:c",
    sep = "\n"
  )
  plan <- glysmith:::.parse_section_plan(output)
  expect_length(plan, 2)
  expect_equal(plan[[1]]$title, "First Section")
  expect_equal(plan[[2]]$title, "Second Section")
})

# Tests for .organize_report_sections error handling
test_that("organize_report_sections returns NULL on AI error", {
  local_mocked_bindings(
    .ask_ai = function(...) stop("AI error"),
    .package = "glysmith"
  )

  expect_warning(result <- glysmith:::.organize_report_sections(
    list(list(id = "step1", label = "Step 1")),
    list(),
    "key"
  ))
  expect_null(result)
})

# Tests for .default_report_sections
test_that("default_report_sections groups plots separately", {
  step_reports <- list(
    list(id = "step1", label = "Step 1", content = "content")
  )
  plot_entries <- list(
    list(id = "plot1", label = "Plot 1", description = "desc", plot = ggplot2::ggplot())
  )

  sections <- glysmith:::.default_report_sections(step_reports, plot_entries)
  expect_length(sections, 2)
  expect_equal(sections[[1]]$title, "Analysis narrative")
  expect_equal(sections[[2]]$title, "Plots")
})

test_that("default_report_sections skips empty steps", {
  step_reports <- list(
    list(id = "step1", label = "Step 1", content = "")  # Empty content
  )
  plot_entries <- list()

  sections <- glysmith:::.default_report_sections(step_reports, plot_entries)
  expect_length(sections, 0)  # No sections because step has no content
})

test_that("default_report_sections handles empty inputs", {
  sections <- glysmith:::.default_report_sections(list(), list())
  expect_length(sections, 0)
})

# Tests for .step_entry and .plot_entry
test_that(".step_entry creates correct structure", {
  step <- list(id = "test", label = "Test Step", content = "Test content")
  entry <- glysmith:::.step_entry(step)
  expect_equal(entry$type, "step")
  expect_equal(entry$id, "test")
  expect_equal(entry$label, "Test Step")
  expect_true(entry$has_content)
})

test_that(".step_entry handles missing content", {
  step <- list(id = "test", label = "Test Step", content = NULL)
  entry <- glysmith:::.step_entry(step)
  expect_false(entry$has_content)
})

test_that(".plot_entry creates correct structure", {
  plot_obj <- list(id = "plot1", label = "Plot 1", description = "desc", plot = ggplot2::ggplot())
  entry <- glysmith:::.plot_entry(plot_obj)
  expect_equal(entry$type, "plot")
  expect_equal(entry$id, "plot1")
  expect_equal(entry$label, "Plot 1")
})

# Tests for .has_report_content
test_that(".has_report_content correctly identifies content", {
  expect_true(glysmith:::.has_report_content("valid content"))
  expect_false(glysmith:::.has_report_content(""))
  expect_false(glysmith:::.has_report_content(NULL))
  expect_false(glysmith:::.has_report_content(character(0)))
  expect_false(glysmith:::.has_report_content(c("a", "b")))  # Multiple elements
})

# Tests for .section_titles
test_that(".section_titles extracts unique titles", {
  plan <- list(
    list(title = "Section 1"),
    list(title = "Section 2"),
    list(title = "Section 1")  # Duplicate
  )
  titles <- glysmith:::.section_titles(plan)
  expect_equal(titles, c("Section 1", "Section 2"))
})

# Tests for .plan_step_sections and .plan_plot_sections
test_that(".plan_step_sections maps steps to sections", {
  plan <- list(
    list(
      title = "Analysis",
      items = list(
        list(type = "step", id = "step1"),
        list(type = "step", id = "step2")
      )
    )
  )
  step_sections <- glysmith:::.plan_step_sections(plan, c("step1", "step2", "step3"))
  expect_equal(step_sections[["step1"]], "Analysis")
  expect_equal(step_sections[["step2"]], "Analysis")
  # step3 is not in the plan, so it should not be in step_sections
  expect_false("step3" %in% names(step_sections))
})

test_that(".plan_plot_sections maps plots to sections", {
  plan <- list(
    list(
      title = "Results",
      items = list(
        list(type = "plot", id = "volcano")
      )
    )
  )
  plot_sections <- glysmith:::.plan_plot_sections(plan, c("volcano", "heatmap"))
  expect_equal(plot_sections[["volcano"]], "Results")
  # heatmap is not in the plan, so it should not be in plot_sections
  expect_false("heatmap" %in% names(plot_sections))
})

# Tests for .describe_plot_ai
test_that(".describe_plot_ai returns description when plot is NULL", {
  result <- glysmith:::.describe_plot_ai(NULL, "label", "existing desc", "key")
  expect_equal(result, "existing desc")
})

test_that(".describe_plot_ai handles AI error gracefully", {
  local_mocked_bindings(
    .ask_ai_multimodal = function(...) stop("AI error"),
    .package = "glysmith"
  )

  expect_warning(result <- glysmith:::.describe_plot_ai(ggplot2::ggplot(), "label", "existing desc", "key"))
  expect_equal(result, "existing desc")
})
