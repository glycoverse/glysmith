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
bp <- blueprint(fork(step_ident_overview(), step_pca(), step_dea_limma()))
result <- forge_analysis(experiment, blueprint = bp)
stopifnot(
  inherits(result, "glysmith_result"),
  identical(result$meta$steps, paste0("node_", 1:3)),
  length(unique(result$tables$node_2__pca_samples[[".sample"]])) ==
    ncol(abundance),
  nrow(result$tables$node_3__dea) > 0L,
  "node_2__pca_scores" %in% names(result$plots)
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
