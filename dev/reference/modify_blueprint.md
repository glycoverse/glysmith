# Modify a Blueprint using Natural Language

**\[experimental\]** Ask a Large Language Model (LLM) to modify an
existing blueprint for glycomics or glycoproteomics data analysis.
DeepSeek is used by default for backward compatibility. Other `ellmer`
providers can be selected with `provider`, `model`, and
provider-specific API key configuration.

## Usage

``` r
modify_blueprint(
  bp,
  description,
  qa_history = NULL,
  exp = NULL,
  group_col = "group",
  model = getOption("glysmith.ai_model", NULL),
  max_retries = 3,
  provider = getOption("glysmith.ai_provider", "deepseek"),
  api_key = getOption("glysmith.ai_api_key", NULL),
  base_url = getOption("glysmith.ai_base_url", NULL)
)
```

## Arguments

- bp:

  A `glysmith_blueprint` object.

- description:

  A description of how you want to modify the blueprint.

- qa_history:

  Character vector of Q&A pairs from
  [`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md).

- exp:

  Optional. A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  or
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  object that provides more context to the LLM.

- group_col:

  The column name of the group variable in `colData(exp)`. Defaults to
  `"group"`.

- model:

  Model to use. Defaults to `getOption("glysmith.ai_model")`, or
  "deepseek-chat" for DeepSeek and the provider default for other
  providers.

- max_retries:

  Maximum number of retries when the AI output is invalid. Default to 3.

- provider:

  AI provider passed to `ellmer`. One of "deepseek", "openai",
  "anthropic", "gemini", "openrouter", or "openai_compatible". Defaults
  to `getOption("glysmith.ai_provider", "deepseek")`.

- api_key:

  API key for the selected provider. If `NULL`, the provider specific
  environment variable is used. Defaults to
  `getOption("glysmith.ai_api_key")`.

- base_url:

  Optional base URL for custom or OpenAI-compatible endpoints. Defaults
  to `getOption("glysmith.ai_base_url")`.

## Details

LLMs can be unstable. If you get an error, try again with another
description. Make sure to examine the returned blueprint carefully to
ensure it's what you want. This function is a companion of
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md).
If the LLM needs required information to proceed, it may ask clarifying
questions interactively and then retry with your answers.
