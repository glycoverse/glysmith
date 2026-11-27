stopifnot(getRversion() >= "4.6.0", getRversion() < "4.7.0")
packages <- scan("/opt/glysmith/packages.txt", what = character(), quiet = TRUE)
for (package in packages) {
  stopifnot(requireNamespace(package, quietly = TRUE))
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
suppressPackageStartupMessages(library(glysmith))
stopifnot(check_glysmith_deps(action = "error"), rmarkdown::pandoc_available())

# A deterministic, complete log2 assay exercises statistics without downloads.
set.seed(123)
abundance <- matrix(
  stats::rnorm(30 * 8, mean = 12, sd = 1),
  nrow = 30,
  dimnames = list(paste0("g", seq_len(30)), paste0("s", seq_len(8)))
)
abundance[1:10, 5:8] <- abundance[1:10, 5:8] + 3
experiment <- glyexp::GlycomicSE(
  abundance,
  rowData = S4Vectors::DataFrame(
    glycan_composition = rep(
      glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2)),
      nrow(abundance)
    ),
    row.names = rownames(abundance)
  ),
  colData = S4Vectors::DataFrame(
    group = factor(rep(c("A", "B"), each = 4)),
    row.names = colnames(abundance)
  ),
  metadata = list(glycan_type = "N", scale = "log2")
)
bp <- blueprint_default(preprocess = FALSE, traits = FALSE, enrich = FALSE)
result <- forge_analysis(experiment, blueprint = bp)
# The image can target a release or a development commit. Check the exported
# analysis content without depending on a package version's internal node IDs.
tables <- cast_table(result)
scores <- grep("pca_samples$", tables, value = TRUE)
dea <- grep("dea$", tables, value = TRUE)
stopifnot(
  inherits(result, "glysmith_result"),
  length(scores) == 1L,
  length(dea) == 1L,
  length(unique(cast_table(result, scores)[[".sample"]])) == ncol(abundance),
  nrow(cast_table(result, dea)) == nrow(abundance),
  any(endsWith(cast_plot(result), "pca_scores"))
)

# Export and render as the runtime user; CI also runs this without networking.
output <- tempfile("glysmith-smoke-")
dir.create(output)
quench_result(result, file.path(output, "results"), plot_ext = "png")
report <- polish_report(
  result,
  file.path(output, "report.html"),
  open = FALSE,
  use_ai = FALSE
)
stopifnot(
  file.exists(file.path(output, "results", "experiment.rds")),
  length(list.files(
    file.path(output, "results", "plots"),
    pattern = "\\.png$"
  )) >
    0L,
  file.exists(report),
  file.info(report)$size > 1000,
  any(grepl("GlySmith report", readLines(report, warn = FALSE), fixed = TRUE))
)
unlink(output, recursive = TRUE)
cat(
  "glysmith smoke test passed on",
  R.version$platform,
  "with",
  R.version.string,
  "\n"
)
