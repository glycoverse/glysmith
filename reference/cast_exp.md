# Get Plots or Tables from GlySmith Result

Helper functions to get processed experiment, plots or tables from a
glysmith result object. Just a syntax sugar for `$exp`,
`$plots$plot_name` and `$tables$table_name` elements of a glysmith
result object, respectively.

## Usage

``` r
cast_exp(x)

cast_plot(x, name)

cast_table(x, name)
```

## Arguments

- x:

  A glysmith result object.

- name:

  The name of the plot or table to get.

## Value

A glyexp_experiment object, a ggplot object or a tibble.

## Examples

``` r
library(glyexp)
exp <- real_experiment2
result <- forge_analysis(exp)
#> ℹ Identification overview
#> ✔ Identification overview [91ms]
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
#> ✖ Preprocessing [137ms]
#> 
#> ! `step_preprocess()` failed. Error: 
#> ℹ Principal component analysis
#> ✖ Principal component analysis [54ms]
#> 
#> ! `step_pca()` failed. Error: infinite or missing values in 'x'
#> ℹ Differential expression analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential expression analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential expression analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential expression analysis (limma)
#> ✔ Differential expression analysis (limma) [224ms]
#> 
#> ℹ Volcano plot
#> ✔ Volcano plot [669ms]
#> 
#> ℹ Heatmap of significant variables
#> ✔ Heatmap of significant variables [84ms]
#> 
#> ℹ Skipping `step_sig_enrich_go()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Skipping `step_sig_enrich_kegg()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Skipping `step_sig_enrich_reactome()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Derived trait calculation
#> ✔ Derived trait calculation [8.3s]
#> 
#> ℹ Differential trait analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential trait analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential trait analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential trait analysis (limma)
#> ✔ Differential trait analysis (limma) [62ms]
#> 
#> ℹ Heatmap of significant traits
#> ✔ Heatmap of significant traits [43ms]
#> 
cast_exp(result)
#> 
#> ── Glycomics Experiment ────────────────────────────────────────────────────────
#> ℹ Expression matrix: 144 samples, 67 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: glycan_composition <comp>, glycan_structure <struct>
cast_table(result, "summary")
#> # A tibble: 4 × 2
#>   item                       n
#>   <chr>                  <dbl>
#> 1 total_composition       67  
#> 2 total_structure         67  
#> 3 composition_per_sample  52.7
#> 4 structure_per_sample    52.7
```
