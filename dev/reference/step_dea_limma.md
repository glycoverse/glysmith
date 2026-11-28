# Differential analysis using limma

Fit
[`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)
to the incoming experiment. Connect
[`step_filter_sig_vars()`](https://glycoverse.github.io/glysmith/dev/reference/step_filter_sig_vars.md)
to select variables or attach a compatible plot.

## Usage

``` r
step_dea_limma(
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

- p_adj_method:

  A character string specifying the method for multiple testing
  correction. Must be one of the methods supported by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). Default
  is "BH" (Benjamini-Hochberg). Set to NULL to skip p-value adjustment.

- covariate_cols:

  A character vector specifying column names in sample information to
  include as covariates in the limma model. Default is NULL.

- subject_col:

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

- filter_p_adj_cutoff, filter_p_val_cutoff, filter_fc_cutoff:

  Default adjusted p-value, raw p-value and fold-change cutoffs for
  downstream filters.

- ...:

  Additional arguments passed to
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).

## Value

A step producing a fitted differential result.

## Examples

``` r
blueprint(step_dea_limma() %=>% step_filter_sig_vars())
#> Blueprint (root accepts exp)
#> └─ node_1  step_dea_limma() (contrast)
#>   └─ node_2  step_filter_sig_vars() (filtered_exp)
```
