skip_if_glysmith_deps_not_installed <- function(
  blueprint = blueprint_default()
) {
  pkgs <- collect_blueprint_packages(blueprint)
  purrr::walk(pkgs, testthat::skip_if_not_installed)
}

as_test_glyco_se <- function(exp) {
  switch(
    glyexp::get_exp_type(exp),
    glycomics = glyexp::as_glycomic_se(exp),
    glycoproteomics = glyexp::as_glycoproteomic_se(exp)
  )
}

forge_analysis_se <- function(
  exp,
  blueprint = blueprint_default(),
  group_col = "group"
) {
  skip_if_glysmith_deps_not_installed(blueprint)
  if (glyexp::is_experiment(exp)) {
    exp <- as_test_glyco_se(exp)
  }
  glysmith::forge_analysis(exp, blueprint = blueprint, group_col = group_col)
}
