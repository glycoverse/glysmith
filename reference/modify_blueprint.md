# Modify a Blueprint using Natural Language

**\[experimental\]** Ask a Large Language Model (LLM) to modify an
existing blueprint for glycomics or glycoproteomics data analysis. To
use this function, you need to have a DeepSeek API key. You can get a
DeepSeek API key from https://platform.deepseek.com. Then set the
environment variable `DEEPSEEK_API_KEY` to your API key with
`Sys.setenv(DEEPSEEK_API_KEY = "your-api-key")`.

## Usage

``` r
modify_blueprint(
  bp,
  description,
  exp = NULL,
  group_col = "group",
  model = "deepseek-reasoner",
  max_retries = 3
)
```

## Arguments

- bp:

  A `glysmith_blueprint` object.

- description:

  A description of how you want to modify the blueprint.

- exp:

  Optional. A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object to provide more context to the LLM.

- group_col:

  The column name of the group variable in the experiment. Default to
  "group".

- model:

  Model to use. Default to "deepseek-reasoner".

- max_retries:

  Maximum number of retries when the AI output is invalid. Default to 3.

## Details

LLMs can be unstable. If you get an error, try again with another
description. Make sure to examine the returned blueprint carefully to
ensure it's what you want. This function is a companion of
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md).
If the LLM needs required information to proceed, it may ask clarifying
questions interactively and then retry with your answers.
