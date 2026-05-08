# Save GlySmith Result

Save processed experiment, plots and tables of a glysmith result object
to a directory. A `README.md` file will also be generated to describe
the saved outputs.

## Usage

``` r
quench_result(
  x,
  dir,
  plot_ext = "pdf",
  table_ext = "csv",
  plot_width = 5,
  plot_height = 5
)
```

## Arguments

- x:

  A glysmith result object.

- dir:

  The directory to save the result.

- plot_ext:

  The extension of the plot files. Either "pdf", "png" or "svg". Default
  is "pdf".

- table_ext:

  The extension of the table files. Either "csv" or "tsv". Default is
  "csv".

- plot_width:

  The width of the plot in inches. Default is 5.

- plot_height:

  The height of the plot in inches. Default is 5.

## Examples

``` r
if (FALSE) { # \dontrun{
library(glyexp)
exp <- real_experiment2
result <- forge_analysis(exp)
quench_result(result, tempdir())
} # }
```
