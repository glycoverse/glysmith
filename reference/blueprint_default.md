# Default blueprint

This blueprint contains the following steps:

- step_preprocess(): Preprocess the data using
  [`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html).

- step_ident_overview(): Summarize the experiment using
  [`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/count_compositions.html).

- step_pca(): Principal component analysis using
  [`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html),
  and plot the PCA using
  [`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html).

- step_dea(): Differential analysis using `glystats::gly_dea()`.

- step_volcano(): Plot a volcano plot using
  [`glyvis::plot_volcano()`](https://glycoverse.github.io/glyvis/reference/plot_volcano.html).

- step_enrich_go(): Perform GO enrichment analysis using
  [`glystats::gly_enrich_go()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).

- step_enrich_kegg(): Perform KEGG enrichment analysis using
  [`glystats::gly_enrich_kegg()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).

- step_enrich_reactome(): Perform Reactome enrichment analysis using
  [`glystats::gly_enrich_reactome()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).

- step_derive_traits(): Derive traits using
  [`glydet::derive_traits()`](https://glycoverse.github.io/glydet/reference/derive_traits.html).

- step_dta(): Differential trait analysis using
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).

## Usage

``` r
blueprint_default(preprocess = TRUE, enrich = TRUE, traits = TRUE)
```

## Arguments

- preprocess:

  Whether to include
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md).

- enrich:

  Whether to include the enrichment steps, i.e.
  [`step_enrich_go()`](https://glycoverse.github.io/glysmith/reference/step_enrich_go.md),
  [`step_enrich_kegg()`](https://glycoverse.github.io/glysmith/reference/step_enrich_kegg.md),
  and
  [`step_enrich_reactome()`](https://glycoverse.github.io/glysmith/reference/step_enrich_reactome.md).

- traits:

  Whether to include the derived trait analysis steps, i.e.
  [`step_derive_traits()`](https://glycoverse.github.io/glysmith/reference/step_derive_traits.md)
  and
  [`step_dta()`](https://glycoverse.github.io/glysmith/reference/step_dta.md).

## Value

A `glysmith_blueprint` object.

## Examples

``` r
blueprint_default()
#> 
#> ── Blueprint (10 steps) ──
#> 
#> • preprocess
#> • ident_overview
#> • pca
#> • dea
#> • volcano
#> • enrich_go
#> • enrich_kegg
#> • enrich_reactome
#> • derive_traits
#> • dta
```
