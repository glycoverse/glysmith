# Step: Quantify Dynamic Motifs

Quantify glycan motifs using
[`glydet::quantify_motifs()`](https://glycoverse.github.io/glydet/reference/quantify_motifs.html)
with
[`glymotif::dynamic_motifs()`](https://glycoverse.github.io/glymotif/reference/dynamic_motifs.html).
This extracts all possible motifs from glycan structures. Works with any
glycan type.

This step requires `exp` (experiment data).

## Usage

``` r
step_quantify_dynamic_motifs(max_size = 3, method = "relative")
```

## Arguments

- max_size:

  Maximum size of motifs to extract. Default is 3.

- method:

  Method for motif quantification ("relative" or "absolute"). Default is
  "relative".

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to quantify motifs for

Data generated:

- `dynamic_motif_exp`: The experiment with quantified motifs

Tables generated:

- `dynamic_motifs`: A table containing the quantified motifs.

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step if motif analysis is needed for non-N-glycans or
  when comprehensive motif extraction is desired.

- This step should be followed by DEA and visualization steps.

## See also

[`glydet::quantify_motifs()`](https://glycoverse.github.io/glydet/reference/quantify_motifs.html),
[`glymotif::dynamic_motifs()`](https://glycoverse.github.io/glymotif/reference/dynamic_motifs.html)

## Examples

``` r
step_quantify_dynamic_motifs()
#> <step "step_quantify_dynamic_motifs()"> Dynamic motif quantification
```
