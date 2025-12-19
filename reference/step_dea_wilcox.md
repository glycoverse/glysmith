# Step: Differential Expression Analysis (DEA) using Wilcoxon test

Run differential analysis using Wilcoxon analysis via
[`glystats::gly_wilcox()`](https://glycoverse.github.io/glystats/reference/gly_wilcox.html).
By default, this runs DEA on the main experiment (`exp`), but can be
configured to run on derived traits (`trait_exp`) or other experiment
objects.

## Usage

``` r
step_dea_wilcox(on = "exp", ...)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to run analysis on. Default
  is `"exp"` for differential expression analysis. Use `"trait_exp"` for
  differential trait analysis.

- ...:

  Step-specific arguments passed to
  [`glystats::gly_wilcox()`](https://glycoverse.github.io/glystats/reference/gly_wilcox.html).
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

- `dea`: A table containing the DEA result (if `on = "exp"`, default)

- `dta`: A table containing the DTA result (if `on = "trait_exp"`)

## See also

[`glystats::gly_wilcox()`](https://glycoverse.github.io/glystats/reference/gly_wilcox.html)

## Examples

``` r
step_dea_wilcox()
#> <step "step_dea_wilcox()"> Differential expression analysis (wilcox)
step_dea_wilcox(on = "trait_exp")  # Differential trait analysis
#> <step "step_dea_wilcox(on = \"trait_exp\")"> Differential trait analysis
#> (wilcox)
```
