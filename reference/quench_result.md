# Save GlySmith Result

Save processed experiment, plots and tables of a glysmith result object
to a directory. A `README.md` file will also be generated to describe
the saved outputs, based on metadata in `x$meta`.

## Usage

``` r
quench_result(
  x,
  dir,
  plot_ext = "pdf",
  table_ext = "csv",
  plot_width = 5,
  plot_height = 5
)
```

## Arguments

- x:

  A glysmith result object.

- dir:

  The directory to save the result.

- plot_ext:

  The extension of the plot files. Either "pdf", "png" or "svg". Default
  is "pdf".

- table_ext:

  The extension of the table files. Either "csv" or "tsv". Default is
  "csv".

- plot_width:

  The width of the plot in inches. Default is 5.

- plot_height:

  The height of the plot in inches. Default is 5.

## Value

A glysmith result object.

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
#> ✔ Preprocessing [115ms]
#> 
#> ℹ Identification overview
#> ✔ Identification overview [94ms]
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
#> ✔ Differential expression analysis (limma) [85ms]
#> 
#> ℹ Volcano plot
#> ✔ Volcano plot [558ms]
#> 
#> ℹ Skipping `step_sig_enrich_go()` because input is not a glycoproteomics experiment.
#> ℹ Skipping `step_sig_enrich_kegg()` because input is not a glycoproteomics experiment.
#> ℹ Skipping `step_sig_enrich_reactome()` because input is not a glycoproteomics experiment.
#> ℹ Derived trait calculation
#> ✔ Derived trait calculation [2.5s]
#> 
#> ℹ Differential trait analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential trait analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential trait analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential trait analysis (limma)
#> ✔ Differential trait analysis (limma) [58ms]
#> 
quench_result(result, tempdir())
#> ℹ Directory already exists. Overwrite? [y/N] 
#> ✔ Result saved to /tmp/Rtmp4QEdZM
```
