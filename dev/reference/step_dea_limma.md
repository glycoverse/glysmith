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

This step depends on the `on` parameter (default: `exp`).

- When `on = "exp"`, requires `exp` (usually after
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/dev/reference/step_preprocess.md)).

- When `on = "trait_exp"`, requires `trait_exp` from
  [`step_derive_traits()`](https://glycoverse.github.io/glysmith/dev/reference/step_derive_traits.md).

- When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from
  [`step_quantify_dynamic_motifs()`](https://glycoverse.github.io/glysmith/dev/reference/step_quantify_dynamic_motifs.md).

- When `on = "branch_motif_exp"`, requires `branch_motif_exp` from
  [`step_quantify_branch_motifs()`](https://glycoverse.github.io/glysmith/dev/reference/step_quantify_branch_motifs.md).

## Usage

``` r
step_dea_limma(
  on = "exp",
  p_adj_method = "BH",
  covariate_cols = NULL,
  subject_col = NULL,
  ref_group = NULL,
  contrasts = NULL,
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

  A character string specifying the method for multiple testing
  correction. Must be one of the methods supported by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). Default
  is "BH" (Benjamini-Hochberg). Set to NULL to skip p-value adjustment.

- covariate_cols:

  (Only for
  [`gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html))
  A character vector specifying column names in sample information to
  include as covariates in the limma model. Default is NULL.

- subject_col:

  (Only for
  [`gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html))
  A character string specifying the column name in sample information
  that contains subject identifiers for paired comparisons. Default is
  NULL.

- ref_group:

  A character string specifying the reference group. If NULL (default),
  the first level of the group factor is used as the reference. Only
  used for two-group comparisons.

- contrasts:

  A character vector specifying custom contrasts. If NULL (default), all
  pairwise comparisons are automatically generated, and the levels
  coming first in the factor will be used as the reference group.
  Supports two formats: "group1-group2" or "group1_vs_group2". Use the
  second format if group names contain hyphens. "group1" will be used as
  the reference group.

- filter_p_adj_cutoff:

  Adjusted p-value cutoff for filtering.

- filter_p_val_cutoff:

  Raw p-value cutoff for filtering.

- filter_fc_cutoff:

  Fold change cutoff for filtering.

- ...:

  Additional arguments passed to
  [`limma::lmFit()`](https://rdrr.io/pkg/limma/man/lmFit.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to run DEA on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to run DEA
  on

- `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif
  experiment to run DEA on

- `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif
  experiment to run DEA on

Data generated:

- `dea_res`: The DEA (differential expression analysis) results (if
  `on = "exp"`, default)

- `dta_res`: The DTA (differential trait analysis) results (if
  `on = "trait_exp"`)

- `dynamic_dma_res`: The DMA results (if `on = "dynamic_motif_exp"`)

- `branch_dma_res`: The DMA results (if `on = "branch_motif_exp"`)

- `sig_exp`: The filtered experiment (if `on = "exp"`, default)

- `sig_trait_exp`: The filtered trait experiment (if `on = "trait_exp"`)

- `sig_dynamic_motif_exp`: The filtered dynamic motif experiment (if
  `on = "dynamic_motif_exp"`)

- `sig_branch_motif_exp`: The filtered branch motif experiment (if
  `on = "branch_motif_exp"`)

Tables generated:

- `dea`: A table containing the DEA (differential expression analysis)
  result (if `on = "exp"`, default)

- `dta`: A table containing the DTA (differential trait analysis) result
  (if `on = "trait_exp"`)

- `dynamic_dma`: A table containing the DMA result (if
  `on = "dynamic_motif_exp"`)

- `branch_dma`: A table containing the DMA result (if
  `on = "branch_motif_exp"`)

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Use this step to perform DEA by default, unless the user asks for
  other methods.

## See also

[`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)

## Examples

``` r
step_dea_limma()
#> <step "step_dea_limma()"> Differential expression analysis (limma)
step_dea_limma(on = "trait_exp")  # Differential trait analysis
#> <step "step_dea_limma(on = \"trait_exp\")"> Differential trait analysis (limma)
step_dea_limma(p_adj_method = "BH")
#> <step "step_dea_limma(p_adj_method = \"BH\")"> Differential expression analysis
#> (limma)
```
