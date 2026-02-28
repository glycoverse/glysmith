# Step: Logo Plot

Create a logo plot for glycosylation sites using
[`glyvis::plot_logo()`](https://glycoverse.github.io/glyvis/reference/plot_logo.html).
The logo plot visualizes the amino acid sequence patterns around
glycosylation sites. This step is only applicable for glycoproteomics
experiments.

This step depends on the `on` parameter (default: `exp`).

- When `on = "exp"`, requires `exp` (experiment data).

- When `on = "sig_exp"`, requires `sig_exp` from one of
  [`step_dea_limma()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_limma.md),
  [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_ttest.md),
  [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_wilcox.md),
  [`step_dea_anova()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_anova.md),
  or
  [`step_dea_kruskal()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_kruskal.md).

## Usage

``` r
step_logo(
  on = "exp",
  n_aa = 5L,
  fasta = NULL,
  plot_width = 5,
  plot_height = 3,
  ...
)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to plot. One of "exp",
  "sig_exp". Default is "exp".

- n_aa:

  The number of amino acids to the left and right of the glycosylation
  site. For example, if `n_aa = 5`, the resulting sequence will contain
  11 amino acids.

- fasta:

  The path to the FASTA file containing protein sequences. If
  [`glyclean::add_site_seq()`](https://glycoverse.github.io/glyclean/reference/add_site_seq.html)
  has been called on the experiment, this argument can be omitted. When
  `site_sequence` is missing and `fasta` is `NULL`, UniProt.ws is used
  to fetch protein sequences automatically.

- plot_width:

  Width of the plot in inches. Default is 5.

- plot_height:

  Height of the plot in inches. Default is 3.

- ...:

  Additional arguments passed to
  [`ggseqlogo::ggseqlogo()`](https://rdrr.io/pkg/ggseqlogo/man/ggseqlogo.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- Depends on `on` parameter (default: `exp`)

Plots generated:

- `logo`: A logo plot (if `on = "exp"`)

- `sig_logo`: A logo plot (if `on = "sig_exp"`)

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Include this step if the user explicitly asks for logo plot.

- If used, ask user if a FASTA file is provided. Tell the user that if
  not, protein sequences will be fetched from Uniprot automatically.

## See also

[`glyvis::plot_logo()`](https://glycoverse.github.io/glyvis/reference/plot_logo.html)

## Examples

``` r
step_logo()
#> <step "step_logo()"> Logo plot
step_logo(fasta = "proteins.fasta")
#> <step "step_logo(fasta = \"proteins.fasta\")"> Logo plot
step_logo(on = "sig_exp")
#> <step "step_logo(on = \"sig_exp\")"> Logo plot of significant variables
```
