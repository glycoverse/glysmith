# Step: Cox Proportional Hazards Model

Perform survival analysis by fitting a Cox proportional hazards model
using
[`glystats::gly_cox()`](https://glycoverse.github.io/glystats/reference/gly_cox.html)
for each variable. This step identifies variables associated with
survival outcomes. The upstream step supplies the experiment.

## Usage

``` r
step_cox(time_col = "time", event_col = "event", p_adj_method = "BH", ...)
```

## Arguments

- time_col:

  Column name in sample information containing survival time. Default is
  "time".

- event_col:

  Column name in sample information containing event indicator (1 for
  event, 0 for censoring). Default is "event".

- p_adj_method:

  Method for adjusting p-values. See `p.adjust.methods`. Default is
  "BH". If NULL, no adjustment is performed.

- ...:

  Additional arguments passed to
  [`glystats::gly_cox()`](https://glycoverse.github.io/glystats/reference/gly_cox.html).

## Value

A `glysmith_step` object.

## See also

[`glystats::gly_cox()`](https://glycoverse.github.io/glystats/reference/gly_cox.html),
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)

## Examples

``` r
step_cox()
#> <step "step_cox()"> Cox proportional hazards model
step_cox(time_col = "survival_time", event_col = "death")
#> <step "step_cox(time_col = \"survival_time\", event_col = \"death\")"> Cox
#> proportional hazards model
```
