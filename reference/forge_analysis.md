# Forge an Analysis for Group Comparison

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
  format `pkg.func.arg`. For example, if you want to pass argument
  `p_adj_method = "BH"` to
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)
  in
  [`step_dea()`](https://glycoverse.github.io/glysmith/reference/step_dea.md),
  set `glystats.gly_limma.p_adj_method = "BH"`. Note that arguments
  about group column specification is controlled by `group_col`
  argument, and should not be passed to `...`.

## Value

A `glysmith_result` object, with the following components:

- `exp`: the experiment after preprocessing.

- `plots`: a named list of ggplot objects.

- `tables`: a named list of tibbles.

- `meta`: a named list of metadata. Currently two elements:

  - `explanation`: a named character vector or list of explanations for
    each plot and table, with keys like `tables$summary` and
    `plots$pca`.

  - `steps`: a character vector of the steps of the analysis.

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
#> ! Step 'preprocess' failed. Skipping... Error: 
#> ℹ Preprocessing
#> ✔ Preprocessing [127ms]
#> 
#> ℹ Identification overview
#> ✔ Identification overview [127ms]
#> 
#> ℹ Principal component analysis
#> ! Step 'pca' failed. Skipping... Error: cannot rescale a constant/zero column to unit variance
#> ℹ Principal component analysis
#> ✔ Principal component analysis [14ms]
#> 
#> ℹ Differential expression analysis
#> ℹ Number of groups: 4
#> ℹ Differential expression analysis
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential expression analysis
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential expression analysis
#> Warning: Partial NA coefficients for 7 probe(s)
#> ✔ Differential expression analysis [60ms]
#> 
#> ℹ Derived trait calculation
#> ✔ Derived trait calculation [2.8s]
#> 
#> ℹ Differential trait analysis
#> ℹ Number of groups: 4
#> ℹ Differential trait analysis
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential trait analysis
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential trait analysis
#> ✔ Differential trait analysis [59ms]
#> 
print(result)
#> 
#> ── GlySmith Analysis Result 
#> Plots: 0, Tables: 4
```
