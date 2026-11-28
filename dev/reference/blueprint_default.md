# Default group-comparison blueprint

Summarize the input and analyze preprocessed abundances with QC, PCA and
limma. Downstream routes visualize the fit and the significant
variables. Optional trait analysis fits a separate model to the derived
experiment.

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

  Whether to include the derived trait analysis steps, including
  [`step_derive_traits()`](https://glycoverse.github.io/glysmith/dev/reference/step_derive_traits.md)
  and
  [`step_dea_limma()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_limma.md).

## Value

A `glysmith_blueprint` object.

## Examples

``` r
blueprint_default()
#> Blueprint (root accepts exp)
#> ├─ node_1  step_ident_overview() [overview] (terminal)
#> └─ node_2  step_preprocess() (exp)
#>   ├─ node_3  step_plot_qc(when = "post") [qc] (terminal)
#>   ├─ node_4  step_pca() [scores] (terminal)
#>   ├─ node_5  step_dea_limma() [abundances] (contrast)
#>   │ ├─ node_6  step_volcano() [abundances] (terminal)
#>   │ ├─ node_7  step_filter_sig_vars() [abundances] (filtered_exp)
#>   │ │ └─ node_8  step_heatmap() [abundances] (terminal)
#>   │ ├─ node_9  step_sig_enrich_go() [abundances] (terminal)
#>   │ ├─ node_10  step_sig_enrich_kegg() [abundances] (terminal)
#>   │ └─ node_11  step_sig_enrich_reactome() [abundances] (terminal)
#>   └─ node_12  step_derive_traits() [traits] (exp)
#>     └─ node_13  step_dea_limma() [traits] (contrast)
#>       └─ node_14  step_filter_sig_vars() [traits] (filtered_exp)
#>         └─ node_15  step_heatmap() [traits] (terminal)
```
