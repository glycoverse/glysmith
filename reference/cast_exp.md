# Get Data from GlySmith Result

Helper functions to get processed experiment, plots, tables or data from
a glysmith result object.

## Usage

``` r
cast_exp(x)

cast_plot(x, name = NULL)

cast_table(x, name = NULL)

cast_data(x, name = NULL)
```

## Arguments

- x:

  A glysmith result object.

- name:

  The name of the plot or table to get. If not specified, return
  available names.

## Value

- `cast_exp()`: a
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html).

- `cast_plot()`: a
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- `cast_table()`: a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

- `cast_data()`: can be any R object.

## Examples

``` r
library(glyexp)
exp <- real_experiment2
result <- forge_analysis(exp)
#> ℹ Identification overview
#> ✔ Identification overview [86ms]
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
#> ✔ Preprocessing [6s]
#> 
#> ℹ Principal component analysis
#> ✔ Principal component analysis [463ms]
#> 
#> ℹ Differential expression analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential expression analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential expression analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential expression analysis (limma)
#> ✔ Differential expression analysis (limma) [266ms]
#> 
#> ℹ Volcano plot
#> ✔ Volcano plot [585ms]
#> 
#> ℹ Heatmap of significant variables
#> ✔ Heatmap of significant variables [45ms]
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
#> ✔ Differential trait analysis (limma) [55ms]
#> 
#> ℹ Heatmap of significant traits
#> ✔ Heatmap of significant traits [39ms]
#> 
cast_exp(result)
#> 
#> ── Glycomics Experiment ────────────────────────────────────────────────────────
#> ℹ Expression matrix: 144 samples, 57 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: glycan_composition <comp>, glycan_structure <struct>
cast_table(result)
#> [1] "summary"         "pca_samples"     "pca_variables"   "pca_eigenvalues"
#> [5] "dea"             "derived_traits"  "dta"            
cast_table(result, "summary")
#> # A tibble: 4 × 2
#>   item                       n
#>   <chr>                  <dbl>
#> 1 total_composition       67  
#> 2 total_structure         67  
#> 3 composition_per_sample  52.7
#> 4 structure_per_sample    52.7
```
