#' Check glysmith dependencies for a blueprint
#'
#' Checks whether the packages required by steps in a blueprint are installed.
#' This does not install or check every package listed in `Suggests`; it only
#' checks the packages declared by the steps in `blueprint`.
#'
#' @param blueprint A [blueprint()]. Defaults to [blueprint_default()].
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
#' # Check dependencies required by the default blueprint
#' check_glysmith_deps()
#'
#' # Check dependencies required by a custom blueprint
#' bp <- blueprint(
#'   step_ident_overview(),
#'   step_pca()
#' )
#' check_glysmith_deps(bp)
#' }
check_glysmith_deps <- function(
  blueprint = blueprint_default(),
  action = c("ask", "error", "note")
) {
  if (!inherits(blueprint, "glysmith_blueprint")) {
    cli::cli_abort(c(
      "`blueprint` must be a {.cls glysmith_blueprint}.",
      "x" = "Got {.cls {class(blueprint)}}."
    ))
  }
  action <- rlang::arg_match(action)

  pkgs <- collect_blueprint_packages(blueprint)
  missing <- pkgs[!purrr::map_lgl(pkgs, rlang::is_installed)]

  switch(
    action,
    ask = {
      if (length(missing) > 0) {
        rlang::check_installed(pkgs)
      } else {
        cli::cli_alert_success("All packages required by the blueprint are installed.")
      }
      invisible(TRUE)
    },
    error = {
      if (length(missing) > 0) {
        cli::cli_abort(c(
          "The following packages required by the blueprint are not installed: {.pkg {missing}}",
          "i" = "Use {.fn check_glysmith_deps} with {.code action = \"ask\"} to install the missing packages."
        ))
      }
      invisible(TRUE)
    },
    note = {
      if (length(missing) > 0) {
        cli::cli_alert_warning(
          "Missing packages required by the blueprint: {.pkg {missing}}"
        )
        cli::cli_inform(
          "Install with: {.code pak::pkg_install(c({paste0('\"', missing, '\"', collapse = ', ')}))}"
        )
        invisible(FALSE)
      } else {
        cli::cli_alert_success("All packages required by the blueprint are installed.")
        invisible(TRUE)
      }
    }
  )
}

#' Collect packages required by a blueprint
#'
#' @param blueprint A `glysmith_blueprint` object.
#'
#' @return Character vector of package names.
#' @noRd
collect_blueprint_packages <- function(blueprint) {
  if (!inherits(blueprint, "glysmith_blueprint")) {
    cli::cli_abort(c(
      "`blueprint` must be a {.cls glysmith_blueprint}.",
      "x" = "Got {.cls {class(blueprint)}}."
    ))
  }

  blueprint |>
    purrr::map("packages", .default = character(0)) |>
    purrr::list_c(ptype = character()) |>
    unique() |>
    sort()
}
