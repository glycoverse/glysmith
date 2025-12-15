# Step: KEGG Enrichment Analysis

Perform KEGG enrichment analysis on differentially expressed variables
using
[`glystats::gly_enrich_kegg()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).
This step requires
[`step_dea()`](https://glycoverse.github.io/glysmith/reference/step_dea.md).

## Usage

``` r
step_enrich_kegg()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_enrich_kegg()
#> <step "enrich_kegg"> KEGG enrichment analysis
```
