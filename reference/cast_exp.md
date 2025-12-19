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
#> ✔ Preprocessing [286ms]
#> 
#> ℹ Identification overview
#> ✔ Identification overview [123ms]
#> 
#> ℹ Principal component analysis
#> ! `step_pca()` failed. Skipping... Error: cannot rescale a constant/zero column to unit variance
#> ℹ Principal component analysis
#> ✔ Principal component analysis [14ms]
#> 
#> ℹ Differential expression analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential expression analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential expression analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential expression analysis (limma)
#> Warning: Partial NA coefficients for 7 probe(s)
#> ✔ Differential expression analysis (limma) [229ms]
#> 
#> ℹ Volcano plot
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the EnhancedVolcano package.
#>   Please report the issue to the authors.
#> Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
#> ℹ Please use the `linewidth` argument instead.
#> ℹ The deprecated feature was likely used in the EnhancedVolcano package.
#>   Please report the issue to the authors.
#> ✔ Volcano plot [746ms]
#> 
#> ℹ Skipping `step_sig_enrich_go()` because input is not a glycoproteomics experiment.
#> ℹ Skipping `step_sig_enrich_kegg()` because input is not a glycoproteomics experiment.
#> ℹ Skipping `step_sig_enrich_reactome()` because input is not a glycoproteomics experiment.
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
#> ✔ Differential trait analysis (limma) [85ms]
#> 
cast_exp(result)
#> 
#> ── Glycomics Experiment ────────────────────────────────────────────────────────
#> ℹ Expression matrix: 144 samples, 67 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: glycan_composition <comp>, glycan_structure <struct>
cast_table(result, "summary")
#> # A tibble: 2 × 2
#>   item            n
#>   <chr>       <int>
#> 1 composition    67
#> 2 structure      67
```
