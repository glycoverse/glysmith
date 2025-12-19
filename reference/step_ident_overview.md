# Step: Identification Overview

Summarize the experiment using
[`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/count_compositions.html).
This step can be run at any time, but is usually run before or right
after preprocessing.

## Usage

``` r
step_ident_overview(...)
```

## Arguments

- ...:

  Step-specific arguments passed to underlying functions. Use the format
  `pkg.func.arg`. For example,
  `step_ident_overview(glyexp.summarize_experiment.count_struct = FALSE)`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to summarize

Tables generated:

- `summary`: A table containing the identification overview of the
  experiment

## See also

[`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/count_compositions.html)

## Examples

``` r
step_ident_overview()
#> <step "step_ident_overview()"> Identification overview
```
