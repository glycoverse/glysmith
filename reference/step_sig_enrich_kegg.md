# Step: KEGG Enrichment Analysis on Differentially Expressed Variables

Perform KEGG enrichment analysis on differentially expressed variables
using
[`glystats::gly_enrich_kegg()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).
This step requires one of the DEA steps to be run. Only execute for
glycoproteomics experiments. Use all genes in OrgDb as the background.

## Usage

``` r
step_sig_enrich_kegg(universe = "all", ...)
```

## Arguments

- universe:

  The universe (background) to use for enrichment analysis. One of "all"
  (all genes in OrgDb), "detected" (detected variables in `exp`).

- ...:

  Step-specific arguments passed to underlying functions. Use the format
  `pkg.func.arg`. For example,
  `step_sig_enrich_kegg(glystats.gly_enrich_kegg.p_adj_method = "BH")`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to perform KEGG enrichment analysis for

- `dea_res`: The DEA results from
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)

Tables generated:

- `kegg_enrich`: A table containing the KEGG enrichment results.

## See also

[`glystats::gly_enrich_kegg()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html)

## Examples

``` r
step_sig_enrich_kegg()
#> <step "step_sig_enrich_kegg()"> KEGG enrichment analysis
```
