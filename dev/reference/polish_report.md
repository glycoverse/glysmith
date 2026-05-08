# Render a HTML Report for GlySmith Results

Generate a self-contained HTML report for a `glysmith_result` object.
The report is rendered via
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
using an internal R Markdown template. If `use_ai` is TRUE, the report
text will be polished, organized into sections, paired with plot
descriptions, and summarized using the configured `ellmer` provider.
DeepSeek is used by default for backward compatibility.

## Usage

``` r
polish_report(
  x,
  output_file,
  title = "GlySmith report",
  open = interactive(),
  use_ai = FALSE,
  ai_provider = getOption("glysmith.ai_provider", "deepseek"),
  ai_model = getOption("glysmith.ai_model", NULL),
  ai_api_key = getOption("glysmith.ai_api_key", NULL),
  ai_base_url = getOption("glysmith.ai_base_url", NULL)
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

  Whether to polish the report text, organize sections, generate plot
  descriptions, and add a summary using AI with the configured `ellmer`
  provider. Default is FALSE.

- ai_provider:

  AI provider passed to `ellmer` when `use_ai = TRUE`. One of
  "deepseek", "openai", "anthropic", "gemini", "openrouter", or
  "openai_compatible". Defaults to
  `getOption("glysmith.ai_provider", "deepseek")`.

- ai_model:

  AI model to use when `use_ai = TRUE`. Defaults to
  `getOption("glysmith.ai_model")`, or "deepseek-chat" for DeepSeek and
  the provider default for other providers.

- ai_api_key:

  API key for the selected provider. If `NULL`, the provider specific
  environment variable is used. Defaults to
  `getOption("glysmith.ai_api_key")`.

- ai_base_url:

  Optional base URL for custom or OpenAI-compatible endpoints. Defaults
  to `getOption("glysmith.ai_base_url")`.

## Value

The normalized path to the generated HTML file.

## Examples

``` r
library(glyexp)
exp <- real_experiment2
result <- forge_analysis(exp)
#> ℹ Identification overview
#> ✔ Identification overview [621ms]
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
#> ── Normalizing data ──
#> ℹ Preprocessing
#> 
#> ℹ Preprocessing
#> ℹ No QC samples found. Using default normalization method based on experiment type.
#> ℹ Preprocessing
#> ℹ Experiment type is "glycomics" with "nrow(exp)" glycans.
#> ℹ Preprocessing
#> ✔ Normalization completed.
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
#> ✔ Preprocessing [4.7s]
#> 
#> ℹ QC (post-preprocessing)
#> ✔ QC (post-preprocessing) [77ms]
#> 
#> ℹ Principal component analysis
#> ✔ Principal component analysis [280ms]
#> 
#> ℹ Differential expression analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential expression analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential expression analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential expression analysis (limma)
#> ✔ Differential expression analysis (limma) [54ms]
#> 
#> ℹ Volcano plot
#> ✔ Volcano plot [351ms]
#> 
#> ℹ Heatmap of significant variables
#> ✔ Heatmap of significant variables [61ms]
#> 
#> ℹ Skipping `step_sig_enrich_go()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Skipping `step_sig_enrich_kegg()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Skipping `step_sig_enrich_reactome()` because input is not a glycoproteomics experiment and input has more than 2 groups.
#> ℹ Derived trait calculation
#> ✔ Derived trait calculation [1.3s]
#> 
#> ℹ Differential trait analysis (limma)
#> ℹ Number of groups: 4
#> ℹ Differential trait analysis (limma)
#> ℹ Groups: "H", "M", "Y", and "C"
#> ℹ Differential trait analysis (limma)
#> ℹ Pairwise comparisons will be performed, with levels coming first as reference groups.
#> ℹ Differential trait analysis (limma)
#> ✔ Differential trait analysis (limma) [45ms]
#> 
#> ℹ Heatmap of significant traits
#> ✔ Heatmap of significant traits [31ms]
#> 
polish_report(result, tempfile(fileext = ".html"), open = FALSE)
#> [1] "/tmp/RtmpssJ80p/file1aca7f8d49eb.html"
```
