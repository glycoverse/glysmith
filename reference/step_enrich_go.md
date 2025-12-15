# Step: GO Enrichment Analysis

Perform GO enrichment analysis on differentially expressed variables
using
[`glystats::gly_enrich_go()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).
This step requires
[`step_dea()`](https://glycoverse.github.io/glysmith/reference/step_dea.md).

## Usage

``` r
step_enrich_go()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_enrich_go()
#> <step "enrich_go"> GO enrichment analysis
```
