# Render a HTML Report for GlySmith Results

Generate a self-contained HTML report for a `glysmith_result` object.
The report is rendered via
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
using an internal R Markdown template. If `use_ai` is TRUE, the report
text will be polished using LLM (deepseek-chat). To use this feature,
you have to provide an API key and set it in the environment variable
`DEEPSEEK_API_KEY` by running
`Sys.setenv(DEEPSEEK_API_KEY = "your_api_key")`. You can apply the API
key on https://platform.deepseek.com.

## Usage

``` r
polish_report(
  x,
  output_file,
  title = "GlySmith report",
  open = interactive(),
  use_ai = FALSE
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

- use_ai:

  Whether to polish the report text using AI (deepseek-chat). Default is
  FALSE.

## Value

The normalized path to the generated HTML file.

## Examples

``` r
library(glyexp)
exp <- real_experiment2
result <- forge_analysis(exp)
#> ℹ Identification overview
#> ✔ Identification overview [88ms]
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
#> ✖ Preprocessing [87ms]
#> 
#> ! `step_preprocess()` failed. Error: 
#> ℹ Principal component analysis
#> ✖ Principal component analysis [19ms]
#> 
#> ! `step_pca()` failed. Error: infinite or missing values in 'x'
#> ℹ Differential expression analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential expression analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential expression analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential expression analysis (limma)
#> ✔ Differential expression analysis (limma) [60ms]
#> 
#> ℹ Volcano plot
#> ✔ Volcano plot [496ms]
#> 
#> ℹ Heatmap of significant variables
#> ✔ Heatmap of significant variables [39ms]
#> 
#> ℹ Skipping `step_sig_enrich_go()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Skipping `step_sig_enrich_kegg()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Skipping `step_sig_enrich_reactome()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Derived trait calculation
#> ✔ Derived trait calculation [2.2s]
#> 
#> ℹ Differential trait analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential trait analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential trait analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential trait analysis (limma)
#> ✔ Differential trait analysis (limma) [89ms]
#> 
#> ℹ Heatmap of significant traits
#> ✔ Heatmap of significant traits [38ms]
#> 
polish_report(result, tempfile(fileext = ".html"), open = FALSE)
#> [1] "/tmp/RtmpxJGSxq/file1d15ce002ee.html"
```
