# Step: Differential Expression Analysis (DEA) using Wilcoxon test

Run differential analysis using Wilcoxon analysis via
[`glystats::gly_wilcox()`](https://glycoverse.github.io/glystats/reference/gly_wilcox.html),
then filter the experiment to keep only the differentially expressed
variables using
[`glystats::filter_sig_vars()`](https://glycoverse.github.io/glystats/reference/filter_sig_vars.html).
By default, this runs DEA on the main experiment (`exp`), but can be
configured to run on derived traits (`trait_exp`) or other experiment
objects. Only use this method for experiments with 2 groups.

## Usage

``` r
step_dea_wilcox(
  on = "exp",
  p_adj_method = "BH",
  ref_group = NULL,
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  ...
)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to run analysis on. Default
  is `"exp"` for differential expression analysis. Use `"trait_exp"` for
  differential trait analysis. Use `"dynamic_motif_exp"` for
  differential dynamic motif analysis. Use `"branch_motif_exp"` for
  differential branch motif analysis.

- p_adj_method:

  A character string specifying the method to adjust p-values. See
  `p.adjust.methods` for available methods. Default is "BH". If NULL, no
  adjustment is performed.

- ref_group:

  A character string specifying the reference group. If NULL (default),
  the first level of the group factor is used as the reference.

- filter_p_adj_cutoff:

  Adjusted p-value cutoff for filtering.

- filter_p_val_cutoff:

  Raw p-value cutoff for filtering.

- filter_fc_cutoff:

  Fold change cutoff for filtering.

- ...:

  Additional arguments passed to
  [`glystats::gly_wilcox()`](https://glycoverse.github.io/glystats/reference/gly_wilcox.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- Depends on `on` parameter (default: `exp`)

Data generated:

- `dea_res`: The DEA results (if `on = "exp"`, default)

- `dta_res`: The DTA results (if `on = "trait_exp"`)

- `dynamic_dma_res`: The DMA results (if `on = "dynamic_motif_exp"`)

- `branch_dma_res`: The DMA results (if `on = "branch_motif_exp"`)

- `sig_exp`: The filtered experiment (if `on = "exp"`, default)

- `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)

- `sig_dynamic_motif_exp`: The filtered dynamic motif experiment (if
  `on = "dynamic_motif_exp"`)

- `sig_branch_motif_exp`: The filtered branch motif experiment (if
  `on = "branch_motif_exp"`)

Tables generated:

- `dea`: A table containing the DEA result (if `on = "exp"`, default)

- `dta`: A table containing the DTA result (if `on = "trait_exp"`)

- `dynamic_dma`: A table containing the DMA result (if
  `on = "dynamic_motif_exp"`)

- `branch_dma`: A table containing the DMA result (if
  `on = "branch_motif_exp"`)

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step only if the user explicitly asks for Wilcoxon test.

- If the experiment has more than 2 groups but the user wants a specific
  two-group comparison, ask which two groups to compare and include
  `step_subset_groups(groups = c("A", "B"))` before this step.

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
