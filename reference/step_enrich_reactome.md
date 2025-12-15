# Step: Reactome Enrichment Analysis

Perform Reactome enrichment analysis on differentially expressed
variables using
[`glystats::gly_enrich_reactome()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).
This step requires
[`step_dea()`](https://glycoverse.github.io/glysmith/reference/step_dea.md).

## Usage

``` r
step_enrich_reactome()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_enrich_reactome()
#> <step "enrich_reactome"> REACTOME enrichment analysis
```
