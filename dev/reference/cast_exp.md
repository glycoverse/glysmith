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

- `cast_exp()`: a
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html).

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
cast_table(result, "summary")
} # }
```
