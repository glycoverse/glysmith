# Aggregate abundances by glycan or glycopeptide identifier

Apply
[`glyclean::aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.html)
to the active experiment. With `to_level = NULL`, glyclean selects the
level from the experiment type and available structural annotations, as
in its former `auto_aggregate()`.

## Usage

``` r
step_aggregate(
  to_level = NULL,
  standardize_variable = TRUE,
  f = sum,
  seed = 123
)
```

## Arguments

- to_level:

  The aggregation level. If `NULL` (the default), glycomics data uses
  "gs" when `glycan_structure` is present and "g" otherwise;
  glycoproteomics data uses "gfs" when `glycan_structure` is present and
  "gf" otherwise. Explicit values are: "g" (glycan compositions), "gs"
  (glycan structures), "gf" (glycoforms), "gp" (glycopeptides), "gfs"
  (glycoforms with structures), or "gps" (glycopeptides with
  structures). See Details for more information.

- standardize_variable:

  Whether to call
  [`glyexp::standardize_variable()`](https://glycoverse.github.io/glyexp/reference/standardize_variable.html)
  after aggregation. Set to `FALSE` to skip network calls for faster
  testing. Default is `TRUE`.

- f:

  A function applied to each group's values separately for each sample.
  Defaults to `sum`. Missing values are removed before calling `f`,
  which must return one numeric value, including for an empty vector
  when all values are missing. For example, use `mean`, `max`, or
  `function(x) mean(x, trim = 0.1)`.

- seed:

  Non-negative integer seed for random number generation. Default is
  `123`. The caller's random state is restored, including on errors.

## Value

A `glysmith_step` object.

## Examples

``` r
step_aggregate()
#> <step "step_aggregate()"> Aggregate abundances
step_aggregate(to_level = "gf", f = mean, standardize_variable = FALSE)
#> <step "step_aggregate(to_level = \"gf\", f = mean, standardize_variable =
#> FALSE)"> Aggregate abundances
```
