# Render a HTML Report for GlySmith Results

Generate a self-contained HTML report for a `glysmith_result` object.
The report is rendered via
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
using an internal R Markdown template.

## Usage

``` r
polish_report(
  x,
  output_file,
  title = "GlySmith report",
  open = interactive(),
  ai_polish = FALSE,
  ai_api_key = NULL
)
```

## Arguments

- x:

  A `glysmith_result` object.

- output_file:

  Path to the output HTML file.

- title:

  Report title.

- open:

  Whether to open the report in a browser after rendering.

- ai_polish:

  Whether to polish the report text using AI (deepseek-chat). Default is
  FALSE.

- ai_api_key:

  API key for the AI model. If NULL, uses the environment variable
  `DEEPSEEK_API_KEY`.

## Value

The normalized path to the generated HTML file.

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
#> ✔ Preprocessing [118ms]
#> 
#> ℹ Identification overview
#> ✔ Identification overview [138ms]
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
#> ✔ Differential expression analysis (limma) [88ms]
#> 
#> ℹ Volcano plot
#> ✔ Volcano plot [797ms]
#> 
#> ℹ Heatmap of significant variables
#> ! `step_heatmap(on = "sig_exp")` failed. Skipping... Error: there is no package called ‘pheatmap’
#> ℹ Heatmap of significant variables
#> ✔ Heatmap of significant variables [15ms]
#> 
#> ℹ Skipping `step_sig_enrich_go()` because input is not a glycoproteomics experiment.
#> ℹ Skipping `step_sig_enrich_kegg()` because input is not a glycoproteomics experiment.
#> ℹ Skipping `step_sig_enrich_reactome()` because input is not a glycoproteomics experiment.
#> ℹ Derived trait calculation
#> ✔ Derived trait calculation [2.3s]
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
#> ! `step_heatmap(on = "sig_trait_exp")` failed. Skipping... Error: there is no package called ‘pheatmap’
#> ℹ Heatmap of significant traits
#> ✔ Heatmap of significant traits [12ms]
#> 
polish_report(result, tempfile(fileext = ".html"), open = FALSE)
#> [1] "/tmp/RtmpRpaCCv/file1f9910d02c83.html"
```
