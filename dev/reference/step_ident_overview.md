# Step: Identification Overview

Summarize the experiment using
[`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/summarize_experiment.html).
This is usually the first step, BEFORE
[`step_preprocess()`](https://glycoverse.github.io/glysmith/dev/reference/step_preprocess.md).
Very light-weight to run, so always include it.

This step requires `exp` (experiment data).

## Usage

``` r
step_ident_overview(count_struct = NULL)
```

## Arguments

- count_struct:

  For counting glycopeptides and glycoforms. whether to count the number
  of glycan structures or glycopeptides. If `TRUE`, glycopeptides or
  glycoforms bearing different glycan structures with the same glycan
  composition are counted as different ones. If not provided (NULL),
  defaults to `TRUE` if `glycan_structure` column exists in the variable
  information tibble, otherwise `FALSE`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to summarize

Tables generated:

- `summary`: A table containing the identification overview of the
  experiment

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Always include this step by default unless the user explicitly
  excludes it.

- Use it as the first step in the blueprint.

## See also

[`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/summarize_experiment.html)

## Examples

``` r
step_ident_overview()
#> <step "step_ident_overview()"> Identification overview
```
