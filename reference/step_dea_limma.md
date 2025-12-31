# Step: Differential Expression Analysis (DEA) using Limma

Run differential analysis using linear model-based analysis via
[`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html),
then filter the experiment to keep only the differentially expressed
variables using
[`glystats::filter_sig_vars()`](https://glycoverse.github.io/glystats/reference/filter_sig_vars.html).
By default, this runs DEA on the main experiment (`exp`), but can be
configured to run on derived traits (`trait_exp`) or other experiment
objects. This step is the recommended DEA method for all experiments,
for both two-group and multi-group experiments.

## Usage

``` r
step_dea_limma(on = "exp", ...)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to run analysis on. Default
  is `"exp"` for differential expression analysis. Use `"trait_exp"` for
  differential trait analysis. Use `"motif_exp"` for differential motif
  analysis.

- ...:

  Step-specific arguments passed to
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).
  Use the format `pkg.func.arg`. For example,
  `step_dea_limma(glystats.gly_limma.p_adj_method = "BH")`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to run DEA on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to run DEA
  on

- `motif_exp` (if `on = "motif_exp"`): The motif experiment to run DEA
  on

Data generated:

- `dea_res`: The DEA (differential expression analysis) results (if
  `on = "exp"`, default)

- `dta_res`: The DTA (differential trait analysis) results (if
  `on = "trait_exp"`)

- `dma_res`: The DMA (differential motif analysis) results (if
  `on = "motif_exp"`)

- `sig_exp`: The filtered experiment (if `on = "exp"`, default)

- `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)

- `sig_motif_exp`: The filtered motif experiment (if `on = "motif_exp"`)

Tables generated:

- `dea`: A table containing the DEA (differential expression analysis)
  result (if `on = "exp"`, default)

- `dta`: A table containing the DTA (differential trait analysis) result
  (if `on = "trait_exp"`)

- `dma`: A table containing the DMA (differential motif analysis) result
  (if `on = "motif_exp"`)

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glystats.gly_limma.p_adj_method`: P-value adjustment method (default:
  "BH").

- `glystats.gly_limma.ref_group`: Reference group for comparison.

- `glystats.gly_limma.contrasts`: Custom contrasts for multi-group
  comparisons.

- `glystats.filter_sig_vars.p_adj_cutoff`: Adjusted p-value cutoff
  (default: 0.05).

- `glystats.filter_sig_vars.p_val_cutoff`: Raw p-value cutoff.

- `glystats.filter_sig_vars.fc_cutoff`: Fold change cutoff.

## See also

[`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)

## Examples

``` r
step_dea_limma()
#> <step "step_dea_limma()"> Differential expression analysis (limma)
step_dea_limma(on = "trait_exp")  # Differential trait analysis
#> <step "step_dea_limma(on = \"trait_exp\")"> Differential trait analysis (limma)
```
