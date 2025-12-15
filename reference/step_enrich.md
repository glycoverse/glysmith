# Step: Enrichment Analysis

Run functional enrichment analysis on differentially expressed variables
using one of:

- [`glystats::gly_enrich_go()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html)

- [`glystats::gly_enrich_kegg()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html)

- [`glystats::gly_enrich_reactome()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html)

This step requires
[`step_dea()`](https://glycoverse.github.io/glysmith/reference/step_dea.md).

## Usage

``` r
step_enrich(kind = c("go", "kegg", "reactome"), retry = 0L)
```

## Arguments

- kind:

  Enrichment type: `"go"`, `"kegg"`, or `"reactome"`.

- retry:

  Number of retries if the step errors.

## Value

A `glysmith_step` object.

A `glysmith_step` object.

## Examples

``` r
step_enrich("go")
#> <step "enrich_go"> GO enrichment analysis
```
