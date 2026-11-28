# Filter the experiment used for a differential fit

In a connected blueprint, attach this step to a differential analysis.
Multiple filters can consume one fit with different cutoffs. A
successful filter can return an experiment with zero rows.

## Usage

``` r
step_filter_sig_vars(
  p_adj_cutoff = 0.05,
  p_val_cutoff = NULL,
  fc_cutoff = NULL,
  ...
)
```

## Arguments

- p_adj_cutoff:

  The threshold for p-adjusted values. Default is 0.05.

- p_val_cutoff:

  The threshold for p-values. We don't recommend using this. Default is
  NULL. If you insist to use it, please set `p_adj_cutoff` to NULL.

- fc_cutoff:

  The threshold for fold changes. Only positive value is needed. For
  example, `2` means fold change \> 2 or \< 1/2. Explicit `NULL`
  disables fold-change filtering for every scale. When omitted, the
  default is `2` for glycoproteomics comparisons with available fold
  changes and `NULL` otherwise. For `"logx"` results, a non-`NULL`
  cutoff warns and is ignored because original-abundance fold changes
  are unavailable; filtering uses only the selected significance cutoff.
  Raw-scale
  [`gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)
  results and other
  [`gly_linear_model()`](https://glycoverse.github.io/glystats/reference/gly_linear_model.html)
  results reject non-`NULL` cutoffs.

- ...:

  Method-specific arguments for
  [`glystats::filter_sig_vars()`](https://glycoverse.github.io/glystats/reference/filter_sig_vars.html),
  such as `on = "post_hoc_test"` and `comparison` for ANOVA.

## Value

A connected step with a filtered experiment as its main output.

## Examples

``` r
blueprint(step_dea_anova() %=>% step_filter_sig_vars())
#> Blueprint (root accepts exp)
#> └─ node_1  step_dea_anova() (omnibus)
#>   └─ node_2  step_filter_sig_vars() (filtered_exp)
```
