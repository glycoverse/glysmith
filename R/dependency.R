#' Check glysmith Suggests dependencies
#'
#' Checks if all packages listed in the Suggests field of DESCRIPTION are
#' installed. If any are missing, prompts the user to install them using `pak`.
#'
#' @param action Character string indicating what to do if packages are missing:
#'   - `"ask"` (default): Prompt the user to install missing packages
#'   - `"error"`: Throw an error if any packages are missing
#'   - `"note"`: Just print which packages are missing, don't prompt
#'
#' @return Returns `TRUE` invisibly if all packages are installed. If
#'   `action = "ask"`, may return `TRUE` after installation or `FALSE` if
#'   user declines.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Check and prompt to install missing packages
#' check_glysmith_deps()
#'
#' # Just report status without prompting
#' check_glysmith_deps(action = "note")
#'
#' # Error if packages are missing
#' check_glysmith_deps(action = "error")
#' }
check_glysmith_deps <- function(action = c("ask", "error", "note")) {
  action <- rlang::arg_match(action)

  suggests_pkgs <- get_suggests_packages()

  switch(action,
    ask = {
      missing <- suggests_pkgs[!rlang::is_installed(suggests_pkgs)]
      if (length(missing) > 0) {
        rlang::check_installed(suggests_pkgs)
      } else {
        cli::cli_alert_success("All Suggests packages are installed.")
      }
      invisible(TRUE)
    },
    error = {
      missing <- suggests_pkgs[!rlang::is_installed(suggests_pkgs)]
      if (length(missing) > 0) {
        cli::cli_abort("The following packages are not installed: {.pkg {missing}}")
      }
      invisible(TRUE)
    },
    note = {
      missing <- suggests_pkgs[!rlang::is_installed(suggests_pkgs)]
      if (length(missing) > 0) {
        cli::cli_alert_warning("Missing Suggests packages: {.pkg {missing}}")
        cli::cli_inform("Install with: {.code pak::pkg_install(c({paste0('\"', missing, '\"', collapse = ', ')}))}")
        invisible(FALSE)
      } else {
        cli::cli_alert_success("All Suggests packages are installed.")
        invisible(TRUE)
      }
    }
  )
}


#' Get Suggests packages from DESCRIPTION
#'
#' Internal helper to read the Suggests field from DESCRIPTION.
#'
#' @return Character vector of package names
#' @keywords internal
get_suggests_packages <- function() {
  if (!rlang::is_installed("desc")) {
    # Fallback: parse DESCRIPTION manually
    desc_path <- system.file("DESCRIPTION", package = "glysmith")
    if (desc_path == "") {
      # Development mode - look in package root
      desc_path <- file.path("DESCRIPTION")
    }
    suggests_field <- read.dcf(desc_path, fields = "Suggests")[1, 1]
    if (is.na(suggests_field)) {
      return(character())
    }
    # Parse comma-separated list, remove version constraints
    pkgs <- strsplit(suggests_field, ",")[[1]]
    pkgs <- trimws(pkgs)
    pkgs <- sub("\\s*\\(.*\\)$", "", pkgs)  # Remove (>= version) constraints
    pkgs <- pkgs[pkgs != ""]
    return(pkgs)
  }

  desc::desc_get_field("Suggests", file = system.file("DESCRIPTION", package = "glysmith")) |>
    strsplit(",") |>
    unlist() |>
    trimws() |>
    sub(pattern = "\\s*\\(.*\\)$", replacement = "") |>
    purrr::keep(~ .x != "")
}
