# Check glysmith Suggests dependencies

Checks if all packages listed in the Suggests field of DESCRIPTION are
installed. If any are missing, prompts the user to install them using
`pak`.

## Usage

``` r
check_glysmith_deps(action = c("ask", "error", "note"))
```

## Arguments

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
# Check and prompt to install missing packages
check_glysmith_deps()

# Just report status without prompting
check_glysmith_deps(action = "note")

# Error if packages are missing
check_glysmith_deps(action = "error")
} # }
```
