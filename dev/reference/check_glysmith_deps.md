# Check glysmith dependencies for a blueprint

Checks whether the packages required by steps in a blueprint are
installed. This does not install or check every package listed in
`Suggests`; it only checks the packages declared by the steps in
`blueprint`.

## Usage

``` r
check_glysmith_deps(
  blueprint = blueprint_default(),
  action = c("ask", "error", "note")
)
```

## Arguments

- blueprint:

  A
  [`blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/blueprint.md).
  Defaults to
  [`blueprint_default()`](https://glycoverse.github.io/glysmith/dev/reference/blueprint_default.md).

- action:

  Character string indicating what to do if packages are missing:

  - `"ask"` (default): Prompt the user to install missing packages

  - `"error"`: Throw an error if any packages are missing

  - `"note"`: Just print which packages are missing, don't prompt

## Value

Returns `TRUE` invisibly if all packages are installed. If
`action = "ask"`, may return `TRUE` after installation or `FALSE` if
user declines.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check dependencies required by the default blueprint
check_glysmith_deps()

# Check dependencies required by a custom blueprint
bp <- blueprint(
  step_ident_overview(),
  step_pca()
)
check_glysmith_deps(bp)
} # }
```
