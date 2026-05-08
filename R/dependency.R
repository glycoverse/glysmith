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
        if (any(is_glycoverse_package(missing))) {
          with_glycoverse_repos(rlang::check_installed(pkgs))
        } else {
          rlang::check_installed(pkgs)
        }
      } else {
        cli::cli_alert_success(
          "All packages required by the blueprint are installed."
        )
      }
      invisible(TRUE)
    },
    error = {
      if (length(missing) > 0) {
        cli::cli_abort(c(
          "The following packages required by the blueprint are not installed: {.pkg {missing}}",
          format_dependency_install_hint(missing)
        ))
      }
      invisible(TRUE)
    },
    note = {
      if (length(missing) > 0) {
        cli::cli_alert_warning(
          "Missing packages required by the blueprint: {.pkg {missing}}"
        )
        cli::cli_inform(format_dependency_install_hint(missing))
        invisible(FALSE)
      } else {
        cli::cli_alert_success(
          "All packages required by the blueprint are installed."
        )
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

#' Format missing dependency installation guidance
#'
#' @param pkgs Character vector of package names.
#'
#' @return Character vector suitable for `cli` messages.
#' @noRd
format_dependency_install_hint <- function(pkgs) {
  pkgs <- sort(unique(pkgs))
  install_line <- paste0(
    "Install with: {.code pak::pkg_install(c(",
    paste0('"', pkgs, '"', collapse = ", "),
    "))}"
  )

  if (!any(is_glycoverse_package(pkgs))) {
    return(c("i" = install_line))
  }

  c(
    "i" = paste0(
      "Add the glycoverse r-universe repository first: {.code ",
      "options(repos = c(glycoverse = \"",
      glycoverse_runiverse_url(),
      "\", CRAN = \"https://cloud.r-project.org\"))}"
    ),
    "i" = install_line
  )
}

#' Check whether packages are part of glycoverse
#'
#' @param pkgs Character vector of package names.
#'
#' @return Logical vector.
#' @noRd
is_glycoverse_package <- function(pkgs) {
  pkgs %in% glycoverse_packages()
}

#' Glycoverse package names
#'
#' @return Character vector.
#' @noRd
glycoverse_packages <- function() {
  c(
    "glyanno",
    "glyclean",
    "glydb",
    "glydet",
    "glyexp",
    "glyfun",
    "glymotif",
    "glyrepr",
    "glystats",
    "glyvis"
  )
}

#' Glycoverse r-universe URL
#'
#' @return Character string.
#' @noRd
glycoverse_runiverse_url <- function() {
  "https://glycoverse.r-universe.dev"
}

#' Evaluate code with glycoverse r-universe available
#'
#' @param expr Expression to evaluate.
#'
#' @return The value of `expr`.
#' @noRd
with_glycoverse_repos <- function(expr) {
  old_repos <- getOption("repos")
  options(repos = dependency_repos(old_repos))
  on.exit(options(repos = old_repos), add = TRUE)
  force(expr)
}

#' Add glycoverse r-universe to a repository vector
#'
#' @param repos Current `repos` option.
#'
#' @return Named character vector of repositories.
#' @noRd
dependency_repos <- function(repos = getOption("repos")) {
  repos <- repos %||% character(0)
  if (length(repos) == 0) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }

  repo_names <- names(repos)
  if (is.null(repo_names)) {
    repo_names <- rep("", length(repos))
  }

  repos <- repos[repo_names != "glycoverse"]
  repo_names <- names(repos)
  if (!"CRAN" %in% repo_names) {
    repos <- c(repos, CRAN = "https://cloud.r-project.org")
  } else if (identical(unname(repos[["CRAN"]]), "@CRAN@")) {
    repos[["CRAN"]] <- "https://cloud.r-project.org"
  }

  c(glycoverse = glycoverse_runiverse_url(), repos)
}
