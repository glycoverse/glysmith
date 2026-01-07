# Step: Differential Expression Analysis (DEA) using ANOVA

Run differential analysis using ANOVA via
[`glystats::gly_anova()`](https://glycoverse.github.io/glystats/reference/gly_anova.html),
then filter the experiment to keep only the differentially expressed
variables using
[`glystats::filter_sig_vars()`](https://glycoverse.github.io/glystats/reference/filter_sig_vars.html).
By default, this runs DEA on the main experiment (`exp`), but can be
configured to run on derived traits (`trait_exp`) or other experiment
objects.

## Usage

``` r
step_dea_anova(
  on = "exp",
  p_adj_method = "BH",
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  filter_on = "main_test",
  filter_comparison = NULL,
  ...
)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to run analysis on. Default
  is `"exp"` for differential expression analysis. Use `"trait_exp"` for
  differential trait analysis. Use `"motif_exp"` for differential motif
  analysis.

- p_adj_method:

  A character string specifying the method to adjust p-values. See
  `p.adjust.methods` for available methods. Default is "BH". If NULL, no
  adjustment is performed.

- filter_p_adj_cutoff:

  Adjusted p-value cutoff for filtering.

- filter_p_val_cutoff:

  Raw p-value cutoff for filtering.

- filter_fc_cutoff:

  Fold change cutoff for filtering.

- filter_on:

  Name of the test to filter on. Default is `"main_test"`. Can also be
  `"post_hoc_test"`.

- filter_comparison:

  Name of the comparison to filter on.

- ...:

  Additional arguments passed to
  [`stats::aov()`](https://rdrr.io/r/stats/aov.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- Depends on `on` parameter (default: `exp`)

Data generated:

- `dea_res`: The DEA results (if `on = "exp"`, default)

- `dta_res`: The DTA results (if `on = "trait_exp"`)

- `dma_res`: The DMA results (if `on = "motif_exp"`)

- `sig_exp`: The filtered experiment (if `on = "exp"`, default)

- `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)

- `sig_motif_exp`: The filtered motif experiment (if `on = "motif_exp"`)

Tables generated:

- `dea_main_test`, `dea_post_hoc_test`: Tables containing the results
  (if `on = "exp"`, default)

- `dta_main_test`, `dta_post_hoc_test`: Tables containing the results
  (if `on = "trait_exp"`)

- `dma_main_test`, `dma_post_hoc_test`: Tables containing the results
  (if `on = "motif_exp"`)

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step only if the user explicitly asks for ANOVA.

## See also

[`glystats::gly_anova()`](https://glycoverse.github.io/glystats/reference/gly_anova.html)

## Examples

``` r
step_dea_anova()
#> <step "step_dea_anova()"> Differential expression analysis (anova)
step_dea_anova(on = "trait_exp")  # Differential trait analysis
#> <step "step_dea_anova(on = \"trait_exp\")"> Differential trait analysis (anova)
```
