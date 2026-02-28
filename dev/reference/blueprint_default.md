# Default blueprint

This blueprint contains the following steps:

- step_ident_overview(): Summarize the experiment using
  [`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/summarize_experiment.html).

- step_preprocess(): Preprocess the data using
  [`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html).

- step_plot_qc(when = "post"): Plot QC plots using
  `glyclean::plot_qc()`.

- step_pca(): Principal component analysis using
  [`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html),
  and plot the PCA using
  [`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html).

- step_dea_limma(): Differential analysis using
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).

- step_volcano(): Plot a volcano plot using
  [`glyvis::plot_volcano()`](https://glycoverse.github.io/glyvis/reference/plot_volcano.html).

- step_heatmap(on = "sig_exp"): Plot a heatmap using
  [`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).

- step_sig_enrich_go(): Perform GO enrichment analysis using
  [`glystats::gly_enrich_go()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).

- step_sig_enrich_kegg(): Perform KEGG enrichment analysis using
  [`glystats::gly_enrich_kegg()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).

- step_sig_enrich_reactome(): Perform Reactome enrichment analysis using
  [`glystats::gly_enrich_reactome()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).

- step_derive_traits(): Derive traits using
  [`glydet::derive_traits()`](https://glycoverse.github.io/glydet/reference/derive_traits.html).

- step_dea_limma(on = "trait_exp"): Differential trait analysis using
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).

- step_heatmap(on = "sig_trait_exp"): Plot a heatmap using
  [`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).

## Usage

``` r
blueprint_default(preprocess = TRUE, enrich = TRUE, traits = TRUE)
```

## Arguments

- preprocess:

  Whether to include
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/dev/reference/step_preprocess.md).

- enrich:

  Whether to include the enrichment steps, i.e.
  [`step_sig_enrich_go()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_go.md),
  [`step_sig_enrich_kegg()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_kegg.md),
  and
  [`step_sig_enrich_reactome()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_reactome.md).

- traits:

  Whether to include the derived trait analysis steps, i.e.
  [`step_derive_traits()`](https://glycoverse.github.io/glysmith/dev/reference/step_derive_traits.md)
  and `step_dea_limma(on = "trait_exp")`.

## Value

A `glysmith_blueprint` object.

## Examples

``` r
blueprint_default()
#> 
#> ── Blueprint (13 steps) ──
#> 
#> • step_ident_overview()
#> • step_preprocess()
#> • step_plot_qc(when = "post")
#> • step_pca()
#> • step_dea_limma()
#> • step_volcano()
#> • step_heatmap(on = "sig_exp")
#> • step_sig_enrich_go()
#> • step_sig_enrich_kegg()
#> • step_sig_enrich_reactome()
#> • step_derive_traits()
#> • step_dea_limma(on = "trait_exp")
#> • step_heatmap(on = "sig_trait_exp")
```
