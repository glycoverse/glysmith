# Create a Blueprint using Natural Language

**\[experimental\]** Ask a Large Language Model (LLM) to create a
blueprint for glycomics or glycoproteomics data analysis. DeepSeek is
used by default for backward compatibility. Other `ellmer` providers can
be selected with `provider`, `model`, and provider-specific API key
configuration.

## Usage

``` r
inquire_blueprint(
  description,
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

- description:

  A description of what you want to analysis.

- exp:

  Optional. A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object to provide more context to the LLM.

- group_col:

  The column name of the group variable in the experiment. Default to
  "group".

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
ensure it's what you want. You can also create parallel analysis
branches with `br("name", step_..., step_...)`, which will namespace
outputs with the branch prefix. If the LLM needs required information to
proceed, it may ask clarifying questions interactively and then retry
with your answers. After a blueprint is generated, the description is
printed and, in interactive sessions, you can press ENTER to accept it
or type new requirements to refine the blueprint. This review step can
repeat until you accept the plan.

Here are some examples that works:

- "I want to know what pathways are enriched for my differentially
  expressed glycoforms."

- "I want a heatmap and a pca plot. I have already performed
  preprocessing myself."

- "I have a glycomics dataset. I want to calculate derived traits and
  perform DEA on them."
