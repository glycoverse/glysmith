# Create a Branch in a Blueprint

Use `br()` to group steps that should run as an isolated branch with
namespaced outputs prefixed by `<name>__`.

## Usage

``` r
br(name, ...)
```

## Arguments

- name:

  Branch name used as a prefix for outputs.

- ...:

  One or more step objects.

## Value

A branch object used inside
[`blueprint()`](https://glycoverse.github.io/glysmith/reference/blueprint.md).

## Examples

``` r
blueprint(
  step_preprocess(),
  br("limma",
    step_dea_limma(),
    step_volcano()
  ),
  br("ttest",
    step_dea_ttest(),
    step_volcano()
  )
)
#> 
#> ── Blueprint (5 steps) ──
#> 
#> • step_preprocess()
#> • br("limma")
#>   • step_dea_limma()
#>   • step_volcano()
#> • br("ttest")
#>   • step_dea_ttest()
#>   • step_volcano()
```
