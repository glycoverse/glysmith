# Perform the Whole Analysis Pipeline

This function performs a comprehensive analysis for group comparison.

## Usage

``` r
forge_analysis(exp, blueprint = blueprint_default(), group_col = "group", ...)
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.

- blueprint:

  A `glysmith_blueprint` object. Default is
  [`blueprint_default()`](https://glycoverse.github.io/glysmith/reference/blueprint_default.md).

- group_col:

  Column name of group information in the sample information. Used for
  various analyses. Default is "group".

- ...:

  Additional arguments passed to the underlying functions. Use the
  format `step_id.pkg.func.arg` (step-scoped). For example, if you want
  to pass argument `p_adj_method = "BH"` to
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)
  in
  [`step_dea_limma()`](https://glycoverse.github.io/glysmith/reference/step_dea_limma.md),
  set `dea_limma.glystats.gly_limma.p_adj_method = "BH"`. To pass
  `batch_col` to
  [`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html)
  in
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md),
  set `preprocess.glyclean.auto_clean.batch_col = "batch"`. Note that
  arguments about group column specification is controlled by
  `group_col` argument, and should not be passed to `...`.

## Value

A `glysmith_result` object, with the following components:

- `exp`: the experiment after preprocessing.

- `plots`: a named list of ggplot objects.

- `tables`: a named list of tibbles.

- `meta`: a named list of metadata, containing:

  - `explanation`: a named character vector or list of explanations for
    each plot and table, with keys like `tables$summary` and
    `plots$pca`.

  - `steps`: a character vector of the steps of the analysis.

  - `log`: the messages and outputs from each step.

- `blueprint`: the blueprint used for the analysis.

## Examples

``` r
exp <- glyexp::real_experiment2
result <- forge_analysis(exp)
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ── Removing variables with too many missing values ──
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ℹ No QC samples found. Using all samples.
#> ℹ Preprocessing
#> ℹ Applying preset "discovery"...
#> ℹ Preprocessing
#> ℹ Total removed: 10 (14.93%) variables.
#> ℹ Preprocessing
#> ✔ Variable removal completed.
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ── Normalizing data ──
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ℹ No QC samples found. Using default normalization method based on experiment type.
#> ℹ Preprocessing
#> ℹ Experiment type is "glycomics". Using `normalize_median_quotient()` + `normalize_total_area()`.
#> ℹ Preprocessing
#> ✔ Normalization completed.
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ── Normalizing data (Total Area) ──
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ✔ Total area normalization completed.
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ── Imputing missing values ──
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ℹ No QC samples found. Using default imputation method based on sample size.
#> ℹ Preprocessing
#> ℹ Sample size > 100, using `impute_miss_forest()`.
#> ℹ Preprocessing
#> ! `step_preprocess()` failed. Skipping... Error: 
#> ℹ Preprocessing
#> ✔ Preprocessing [115ms]
#> 
#> ℹ Identification overview
#> ✔ Identification overview [120ms]
#> 
#> ℹ Principal component analysis
#> ! `step_pca()` failed. Skipping... Error: cannot rescale a constant/zero column to unit variance
#> ℹ Principal component analysis
#> ✔ Principal component analysis [13ms]
#> 
#> ℹ Differential expression analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential expression analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential expression analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential expression analysis (limma)
#> Warning: Partial NA coefficients for 7 probe(s)
#> ✔ Differential expression analysis (limma) [66ms]
#> 
#> ℹ Volcano plot
#> ✔ Volcano plot [658ms]
#> 
#> ℹ Heatmap of significant variables
#> ! `step_heatmap(on = "sig_exp")` failed. Skipping... Error: there is no package called ‘pheatmap’
#> ℹ Heatmap of significant variables
#> ✔ Heatmap of significant variables [13ms]
#> 
#> ℹ Skipping `step_sig_enrich_go()` because input is not a glycoproteomics experiment.
#> ℹ Skipping `step_sig_enrich_kegg()` because input is not a glycoproteomics experiment.
#> ℹ Skipping `step_sig_enrich_reactome()` because input is not a glycoproteomics experiment.
#> ℹ Derived trait calculation
#> ✔ Derived trait calculation [2.9s]
#> 
#> ℹ Differential trait analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential trait analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential trait analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential trait analysis (limma)
#> ✔ Differential trait analysis (limma) [86ms]
#> 
#> ℹ Heatmap of significant traits
#> ! `step_heatmap(on = "sig_trait_exp")` failed. Skipping... Error: there is no package called ‘pheatmap’
#> ℹ Heatmap of significant traits
#> ✔ Heatmap of significant traits [13ms]
#> 
print(result)
#> 
#> ── GlySmith Analysis Result 
#> Plots: 6, Tables: 4
```
