# Differential analysis using ANOVA

Fit
[`glystats::gly_anova()`](https://glycoverse.github.io/glystats/reference/gly_anova.html)
to the incoming experiment. Connect
[`step_filter_sig_vars()`](https://glycoverse.github.io/glysmith/dev/reference/step_filter_sig_vars.md)
to select variables or attach a compatible plot.

## Usage

``` r
step_dea_anova(
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

- p_adj_method:

  A character string specifying the method to adjust p-values. See
  `p.adjust.methods` for available methods. Default is "BH". If NULL, no
  adjustment is performed.

- filter_p_adj_cutoff, filter_p_val_cutoff, filter_fc_cutoff:

  Default adjusted p-value, raw p-value and fold-change cutoffs for
  downstream filters.

- filter_on:

  Default test to select in a downstream filter, either `"main_test"` or
  `"post_hoc_test"`.

- filter_comparison:

  Default comparison for the post-hoc filter.

- ...:

  Additional arguments passed to
  [`glystats::gly_anova()`](https://glycoverse.github.io/glystats/reference/gly_anova.html).

## Value

A step producing a fitted differential result.

## Examples

``` r
blueprint(step_dea_anova() %=>% step_filter_sig_vars())
#> Blueprint (root accepts exp)
#> └─ node_1  step_dea_anova() (omnibus)
#>   └─ node_2  step_filter_sig_vars() (filtered_exp)
```
