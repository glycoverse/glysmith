local_mock_glycan_fact <- function() {
  testthat::local_mocked_bindings(
    .generate_glycan_fact = function(...) {
      "Do you know that glycans can be branched, creating huge diversity from a small set of monosaccharides?"
    },
    .package = "glysmith",
    .env = rlang::caller_env()
  )
}
