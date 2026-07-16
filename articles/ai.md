# AI-Powered Workflow in Glysmith

`glysmith` leverages the power of Large Language Models (LLMs) to make
glyco-omics analysis more intuitive and efficient. From generating
complex analytical pipelines to crafting comprehensive reports, AI
integration allows you to focus on the science while it handles the
boilerplate.

## Getting Started

To enable AI features, configure an API key for the LLM provider you
want to use. DeepSeek is the default provider, and you can obtain a key
from the [DeepSeek Platform](https://platform.deepseek.com).

Once you have your key, set it as an environment variable in your R
session:

``` r

Sys.setenv(DEEPSEEK_API_KEY = "your_api_key")
```

You can also use other providers supported by `ellmer`. For example,
OpenAI can be used by setting `OPENAI_API_KEY` and passing
`provider = "openai"`:

``` r

Sys.setenv(OPENAI_API_KEY = "your_api_key")
bp <- inquire_blueprint(
  "I want to perform DEA and visualize the results.",
  exp = your_exp,
  provider = "openai",
  model = "gpt-4.1"
)
```

If you prefer to configure the provider once per R session, use
package-level options:

``` r

options(
  glysmith.ai_provider = "openai",
  glysmith.ai_model = "gpt-4.1"
)

bp <- inquire_blueprint(
  "I want to perform DEA and visualize the results.",
  exp = your_exp
)
```

For OpenAI-compatible endpoints, set `OPENAI_API_KEY` and pass both
`provider = "openai_compatible"` and `base_url`. The corresponding
options are `glysmith.ai_provider`, `glysmith.ai_model`,
`glysmith.ai_api_key`, and `glysmith.ai_base_url`.

*Note: This environment variable is session-specific. You will need to
set it again in new R sessions, or add it to your `.Renviron` file for a
permanent setup.*

### Cost and Efficiency

While using LLMs does incur costs, it is extremely affordable for
glysmith workflows. Generating a blueprint or an AI-driven report
typically costs less than **\$0.01 per run**, offering
professional-grade assistance for a fraction of a cent.

``` r

library(glysmith)
```

## Designing Pipelines with `inquire_blueprint()`

Instead of manually browsing and selecting various `step_xxx` functions
and manage the parameters, you can describe your analysis goals in plain
English (or your language).
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
will translate your requirements into a structured analytical blueprint.

``` r

# Describe your goals in natural language
bp <- inquire_blueprint("I want to perform DEA and visualize the results.", exp = your_exp)
print(bp)
```

By providing a `GlycomicSE` or `GlycoproteomicSE` object as `exp`, the
LLM can understand your data structure and experimental design, ensuring
the generated blueprint is tailored to your specific dataset. The
function also prints a brief rationale for the chosen steps to the
console.

After you answering all the questions, a blueprint is returned as `bp`.
You can then pass it to
[`forge_analysis()`](https://glycoverse.github.io/glysmith/reference/forge_analysis.md):

``` r

res <- forge_analysis(exp, blueprint = bp)
```

## Refining Your Analysis

If the initial blueprint needs adjustment, you can use
[`modify_blueprint()`](https://glycoverse.github.io/glysmith/reference/modify_blueprint.md)
to refine it iteratively without starting from scratch.

``` r

# Add or remove steps using natural language
new_bp <- modify_blueprint(bp, "Also include a PCA analysis.", exp = your_exp)
print(new_bp)
```

## AI-Enhanced Reporting

While
[`polish_report()`](https://glycoverse.github.io/glysmith/reference/polish_report.md)
uses robust default rules, setting `use_ai = TRUE` unlocks advanced AI
capabilities for report generation:

``` r

polish_report(result, "report.html", use_ai = TRUE)
```

To use a non-default provider for reporting, pass the `ai_`
configuration arguments:

``` r

polish_report(
  result,
  "report.html",
  use_ai = TRUE,
  ai_provider = "openai",
  ai_model = "gpt-4.1"
)
```

In AI mode, the LLM performs several high-level tasks:

1.  **Structural Optimization**: Dynamically arranges sections, text,
    and figures for optimal flow.
2.  **Contextual Narrative**: Drafts descriptive text for each analysis
    section.
3.  **Multimodal Interpretation**: Uses vision-capable models to
    interpret and explain generated figures.
4.  **Professional Finishing**: Polishes titles and subtitles for a
    publication-ready look.

## Complete AI Workflow

The entire pipeline, from raw data to a finished report, can be
simplified into a few intelligent steps:

``` r

# 1. Generate an analysis plan
bp <- inquire_blueprint("Perform DEA and visualize key findings.", exp = your_exp)

# 2. Execute the analysis
res <- forge_analysis(your_exp, bp)

# 3. Export data and intermediate results
quench_result(res, "analysis_output/")

# 4. Generate an AI-enhanced report
polish_report(res, "report.html", use_ai = TRUE)
```
