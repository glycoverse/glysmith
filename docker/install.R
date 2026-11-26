options(timeout = 1200, Ncpus = as.integer(Sys.getenv("R_BUILD_NCPUS", "2")))
stopifnot(getRversion() >= "4.6.0", getRversion() < "4.7.0")

reference <- Sys.getenv("GLYSMITH_REF", "glysmith")
stopifnot(nzchar(reference))

# The base image supplies pak, the Glycoverse repository, and the core packages.
# Include glysmith's Suggests so its analysis steps can run without installation.
pak::pkg_install(
  reference,
  lib = .Library.site,
  dependencies = TRUE,
  ask = FALSE
)

description <- utils::packageDescription("glysmith")
dependencies <- tools::package_dependencies(
  "glysmith",
  db = utils::installed.packages(),
  which = c("Depends", "Imports", "LinkingTo", "Suggests"),
  recursive = FALSE
)[["glysmith"]]
packages <- sort(unique(c("glysmith", "glyread", dependencies)))
missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Packages failed to load: ", paste(missing, collapse = ", "))
}
stopifnot(rmarkdown::pandoc_available())

writeLines(packages, "/opt/glysmith/packages.txt")
writeLines(description$Version, "/opt/glysmith/version.txt")
writeLines(reference, "/opt/glysmith/package-reference.txt")
installed <- as.data.frame(
  utils::installed.packages(),
  stringsAsFactors = FALSE
)
fields <- intersect(
  c("Package", "Version", "Repository", "Built"),
  names(installed)
)
utils::write.table(
  installed[order(installed$Package), fields, drop = FALSE],
  "/opt/glysmith/package-versions.tsv",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
writeLines(capture.output(sessionInfo()), "/opt/glysmith/build-session.txt")
