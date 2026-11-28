# Differential analysis using a t-test

Fit
[`glystats::gly_ttest()`](https://glycoverse.github.io/glystats/reference/gly_ttest.html)
to the incoming experiment. Connect
[`step_filter_sig_vars()`](https://glycoverse.github.io/glysmith/dev/reference/step_filter_sig_vars.md)
to select variables or attach a compatible plot.

## Usage

``` r
step_dea_ttest(
  p_adj_method = "BH",
  ref_group = NULL,
  filter_p_adj_cutoff = 0.05,
  filter_p_val_cutoff = NULL,
  filter_fc_cutoff = NULL,
  ...
)
```

## Arguments

- p_adj_method:

  A character string specifying the method to adjust p-values. See
  `p.adjust.methods` for available methods. Default is "BH". If NULL, no
  adjustment is performed.

- ref_group:

  A character string specifying the reference group. If NULL (default),
  the first level of the group factor is used as the reference.

- filter_p_adj_cutoff, filter_p_val_cutoff, filter_fc_cutoff:

  Default adjusted p-value, raw p-value and fold-change cutoffs for
  downstream filters.

- ...:

  Additional arguments passed to
  [`glystats::gly_ttest()`](https://glycoverse.github.io/glystats/reference/gly_ttest.html).

## Value

A step producing a fitted differential result.

## Examples

``` r
blueprint(step_dea_ttest() %=>% step_filter_sig_vars())
#> Blueprint (root accepts exp)
#> └─ node_1  step_dea_ttest() (contrast)
#>   └─ node_2  step_filter_sig_vars() (filtered_exp)
```
