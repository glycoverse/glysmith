# Step: Quantify Branch Motifs

Quantify N-glycan branch motifs using
[`glydet::quantify_motifs()`](https://glycoverse.github.io/glydet/reference/quantify_motifs.html)
with
[`glymotif::branch_motifs()`](https://glycoverse.github.io/glymotif/reference/branch_motifs.html).
This extracts specific N-glycan branching patterns (bi-antennary,
tri-antennary, etc.). Only works with N-glycans.

This step requires `exp` (experiment data).

## Usage

``` r
step_quantify_branch_motifs(method = "relative")
```

## Arguments

- method:

  Method for motif quantification ("relative" or "absolute"). Default is
  "relative".

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to quantify motifs for (must be N-glycans)

Data generated:

- `branch_motif_exp`: The experiment with quantified branch motifs

Tables generated:

- `branch_motifs`: A table containing the quantified branch motifs.

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Include this step if motif analysis is needed specifically for
  N-glycans.

- This step should be followed by DEA and visualization steps.

## See also

[`glydet::quantify_motifs()`](https://glycoverse.github.io/glydet/reference/quantify_motifs.html),
[`glymotif::branch_motifs()`](https://glycoverse.github.io/glymotif/reference/branch_motifs.html)

## Examples

``` r
step_quantify_branch_motifs()
#> <step "step_quantify_branch_motifs()"> Branch motif quantification
```
