# Get Data from GlySmith Result

Helper functions to get processed experiment, plots, tables or data from
a glysmith result object.

## Usage

``` r
cast_exp(x)

cast_plot(x, name = NULL)

cast_table(x, name = NULL)

cast_data(x, name = NULL)
```

## Arguments

- x:

  A glysmith result object.

- name:

  The name of the plot or table to get. If not specified, return
  available names.

## Value

- `cast_exp()`: the input experiment for multiple entry routes. For a
  single route, the last experiment on the main route, or the input
  experiment if that route produces no experiment. Trait and motif steps
  can return a derived SummarizedExperiment. Returns NULL if the
  producer of the main-route experiment failed, was skipped or was
  blocked.

- `cast_plot()`: a
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- `cast_table()`: a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

- `cast_data()`: can be any R object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(glyexp)
exp <- real_experiment2
result <- forge_analysis(exp)
cast_exp(result)
cast_table(result)
cast_table(result, "node_1__summary")
} # }
```
