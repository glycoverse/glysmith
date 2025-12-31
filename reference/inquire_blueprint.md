# Create a Blueprint using Natural Language

**\[experimental\]** Ask a Large Language Model (LLM) to create a
blueprint for glycomics or glycoproteomics data analysis. To use this
function, you need to have a DeepSeek API key. You can get a DeepSeek
API key from https://platform.deepseek.com. Then set the environment
variable `DEEPSEEK_API_KEY` to your API key with
`Sys.setenv(DEEPSEEK_API_KEY = "your-api-key")`.

## Usage

``` r
inquire_blueprint(
  description,
  exp = NULL,
  group_col = "group",
  model = "deepseek-reasoner",
  max_retries = 3
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

  Model to use. Default to "deepseek-reasoner".

- max_retries:

  Maximum number of retries when the AI output is invalid. Default to 3.

## Details

LLMs can be unstable. If you get an error, try again with another
description. Make sure to examine the returned blueprint carefully to
ensure it's what you want.

Here are some examples that works:

- "I want to know what pathways are enriched for my differentially
  expressed glycoforms."

- "I want a heatmap and a pca plot. I have already performed
  preprocessing myself."

- "I have a glycomics dataset. I want to calculate derived traits and
  perform DEA on them."
