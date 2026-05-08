skip_if_default_blueprint_integration_disabled <- function() {
  run <- tolower(Sys.getenv(
    "GLYSMITH_INTEGRATION_TEST",
    unset = "false"
  ))
  testthat::skip_if_not(
    run %in% c("true", "1", "yes"),
    paste(
      "Set GLYSMITH_INTEGRATION_TEST=true",
      "to run default-blueprint integration tests."
    )
  )
}
