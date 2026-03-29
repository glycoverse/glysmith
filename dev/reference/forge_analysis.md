# Perform the Whole Analysis Pipeline

This function performs a comprehensive analysis for group comparison.

## Usage

``` r
forge_analysis(exp, blueprint = blueprint_default(), group_col = "group")
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.

- blueprint:

  A `glysmith_blueprint` object. Default is
  [`blueprint_default()`](https://glycoverse.github.io/glysmith/dev/reference/blueprint_default.md).

- group_col:

  Column name of group information in the sample information. Used for
  various analyses. Default is "group".

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
#> ℹ Identification overview
#> ✔ Identification overview [817ms]
#> 
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
#> ✔ Imputation completed.
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ── Correcting batch effects ──
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ℹ Batch column  not found in sample_info. Skipping batch correction.
#> ℹ Preprocessing
#> ✔ Batch correction completed.
#> ℹ Preprocessing
#> ✔ Preprocessing [5.7s]
#> 
#> ℹ QC (post-preprocessing)
#> ✔ QC (post-preprocessing) [98ms]
#> 
#> ℹ Principal component analysis
#> ✔ Principal component analysis [387ms]
#> 
#> ℹ Differential expression analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential expression analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential expression analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential expression analysis (limma)
#> ✔ Differential expression analysis (limma) [74ms]
#> 
#> ℹ Volcano plot
#> ✔ Volcano plot [539ms]
#> 
#> ℹ Heatmap of significant variables
#> ✔ Heatmap of significant variables [43ms]
#> 
#> ℹ Skipping `step_sig_enrich_go()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Skipping `step_sig_enrich_kegg()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Skipping `step_sig_enrich_reactome()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Derived trait calculation
#> ✔ Derived trait calculation [1.9s]
#> 
#> ℹ Differential trait analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential trait analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential trait analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential trait analysis (limma)
#> ✔ Differential trait analysis (limma) [57ms]
#> 
#> ℹ Heatmap of significant traits
#> ✔ Heatmap of significant traits [40ms]
#> 
print(result)
#> 
#> ── GlySmith Analysis Result 
#> Plots: 15, Tables: 7, Data: 7
```
