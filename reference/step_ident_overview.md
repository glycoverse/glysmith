# Step: Identification Overview

Summarize the experiment using
[`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/summarize_experiment.html).
This is usually the first step, BEFORE
[`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md).
Very light-weight to run, so always include it.

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

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glyexp.summarize_experiment.count_struct`: Whether to count by
  structure or composition.

## See also

[`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/summarize_experiment.html)

## Examples

``` r
step_ident_overview()
#> <step "step_ident_overview()"> Identification overview
```
