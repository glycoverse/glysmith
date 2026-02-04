# Step: Quantify Motifs

Quantify glycan motifs using
[`glydet::quantify_motifs()`](https://glycoverse.github.io/glydet/reference/quantify_motifs.html).
The motifs are extracted using `glymotif::branch_motifs()` for N-glycans
and `glymotif::dynamic_motifs()` for others. Advanced glycan motif
analysis that quantify glycan motifs (substructures) of a glycome or
each glycosite. Need glycan structure information.

## Usage

``` r
step_quantify_motifs(max_size = 3, method = "relative")
```

## Arguments

- max_size:

  Maximum size of motifs to extract.

- method:

  Method for motif quantification ("relative" or "absolute").

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to quantify motifs for

Data generated:

- `motif_exp`: The experiment with quantified motifs

Tables generated:

- `quantified_motifs`: A table containing the quantified motifs.

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step if needed.

- This step should be followed by the DEA step and visualization steps.

## See also

[`glydet::quantify_motifs()`](https://glycoverse.github.io/glydet/reference/quantify_motifs.html),
`glymotif::dynamic_motifs()`, `glymotif::branch_motifs()`

## Examples

``` r
step_quantify_motifs()
#> <step "step_quantify_motifs()"> Motif quantification
```
