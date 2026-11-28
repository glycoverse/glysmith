# Step: Logo Plot

Create a logo plot for glycosylation sites using
[`glyvis::plot_logo()`](https://glycoverse.github.io/glyvis/reference/plot_logo.html).
The logo plot visualizes the amino acid sequence patterns around
glycosylation sites. This step is only applicable for glycoproteomics
experiments. The upstream step supplies the experiment.

## Usage

``` r
step_logo(n_aa = 5L, fasta = NULL, plot_width = 5, plot_height = 3, ...)
```

## Arguments

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

  Must be empty. Supply plotting controls by their full argument names.

## Value

A `glysmith_step` object.

## See also

[`glyvis::plot_logo()`](https://glycoverse.github.io/glyvis/reference/plot_logo.html)

## Examples

``` r
step_logo()
#> <step "step_logo()"> Logo plot
step_logo(fasta = "proteins.fasta")
#> <step "step_logo(fasta = \"proteins.fasta\")"> Logo plot
```
