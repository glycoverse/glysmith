skip_if_glysmith_deps_not_installed <- function(blueprint = blueprint_default()) {
  pkgs <- collect_blueprint_packages(blueprint)
  purrr::walk(pkgs, testthat::skip_if_not_installed)
}

forge_analysis <- function(
  exp,
  blueprint = blueprint_default(),
  group_col = "group"
) {
  skip_if_glysmith_deps_not_installed(blueprint)
  glysmith::forge_analysis(exp, blueprint = blueprint, group_col = group_col)
}
