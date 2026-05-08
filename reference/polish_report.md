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
if (FALSE) { # \dontrun{
library(glyexp)
exp <- real_experiment2
result <- forge_analysis(exp)
polish_report(result, tempfile(fileext = ".html"), open = FALSE)
} # }
```
