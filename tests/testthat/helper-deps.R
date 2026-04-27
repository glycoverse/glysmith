skip_if_glysmith_deps_not_installed <- function() {
  pkgs <- get_suggests_packages()
  purrr::walk(pkgs, testthat::skip_if_not_installed)
}

forge_analysis <- function(...) {
  skip_if_glysmith_deps_not_installed()
  glysmith::forge_analysis(...)
}
