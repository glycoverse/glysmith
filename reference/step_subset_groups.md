# Step: Subset Groups

Subset the experiment to specific groups using the `group` column in
sample information. This is useful when downstream steps require exactly
two groups for comparison. Usually run after
[`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md)
and before DEA or enrichment steps.

This step requires `exp` (experiment data).

## Usage

``` r
step_subset_groups(groups = NULL)
```

## Arguments

- groups:

  Group names to keep. If `NULL`, this step will be skipped.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to subset

Data generated:

- `full_exp`: The original experiment before subsetting

This step overwrites `exp` in the context with the subset experiment.

## Examples

``` r
step_subset_groups(groups = c("H", "C"))
#> <step "step_subset_groups(groups = c(\"H\", \"C\"))"> Subset groups
```
