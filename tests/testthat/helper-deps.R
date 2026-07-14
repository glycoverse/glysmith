skip_if_glysmith_deps_not_installed <- function(
  blueprint = blueprint_default()
) {
  pkgs <- collect_blueprint_packages(blueprint)
  purrr::walk(pkgs, testthat::skip_if_not_installed)
}

as_test_glyco_se <- function(exp) {
  if (methods::is(exp, "SummarizedExperiment")) {
    return(exp)
  }
  switch(
    S4Vectors::metadata(glyexp::as_se(exp))[["exp_type"]],
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
  if (inherits(exp, "glyexp_experiment")) {
    exp <- as_test_glyco_se(exp)
  }
  glysmith::forge_analysis(exp, blueprint = blueprint, group_col = group_col)
}
