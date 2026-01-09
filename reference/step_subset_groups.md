# Step: Subset Groups

Subset the experiment to specific groups using the `group` column in
sample information. This is useful when downstream steps require exactly
two groups for comparison. Usually run after
[`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md)
and before DEA or enrichment steps.

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

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Use this step when the experiment has more than 2 groups but the user
  wants a specific two-group comparison.

- Ask the user which two groups to compare, and place this step before
  DEA and enrichment steps.

- Use the order of the user-provided groups to set factor levels.

## Examples

``` r
step_subset_groups(groups = c("H", "C"))
#> <step "step_subset_groups(groups = c(\"H\", \"C\"))"> Subset groups
```
