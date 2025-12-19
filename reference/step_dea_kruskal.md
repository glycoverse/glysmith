# Step: Differential Expression Analysis (DEA) using Kruskal-Wallis test

Run differential analysis using Kruskal-Wallis analysis via
[`glystats::gly_kruskal()`](https://glycoverse.github.io/glystats/reference/gly_kruskal.html).
By default, this runs DEA on the main experiment (`exp`), but can be
configured to run on derived traits (`trait_exp`) or other experiment
objects.

## Usage

``` r
step_dea_kruskal(on = "exp", ...)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to run analysis on. Default
  is `"exp"` for differential expression analysis. Use `"trait_exp"` for
  differential trait analysis.

- ...:

  Step-specific arguments passed to
  [`glystats::gly_kruskal()`](https://glycoverse.github.io/glystats/reference/gly_kruskal.html).
  Use the format `pkg.func.arg`.

## Value

A `glysmith_step` object.

## Details

Data required:

- Depends on `on` parameter (default: `exp`)

Data generated:

- `dea_res`: The DEA results (if `on = "exp"`, default)

- `dta_res`: The DTA results (if `on = "trait_exp"`)

Tables generated:

- `dea_main_test`, `dea_post_hoc_test`: Tables containing the results
  (if `on = "exp"`, default)

- `dta_main_test`, `dta_post_hoc_test`: Tables containing the results
  (if `on = "trait_exp"`)

## See also

[`glystats::gly_kruskal()`](https://glycoverse.github.io/glystats/reference/gly_kruskal.html)

## Examples

``` r
step_dea_kruskal()
#> <step "step_dea_kruskal()"> Differential expression analysis (kruskal)
step_dea_kruskal(on = "trait_exp")  # Differential trait analysis
#> <step "step_dea_kruskal(on = \"trait_exp\")"> Differential trait analysis
#> (kruskal)
```
