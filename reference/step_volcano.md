# Step: Volcano Plot

Create a volcano plot from DEA results using
[`glyvis::plot_volcano()`](https://glycoverse.github.io/glyvis/reference/plot_volcano.html).
This step requires
[`step_dea()`](https://glycoverse.github.io/glysmith/reference/step_dea.md).

## Usage

``` r
step_volcano()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_volcano()
#> $id
#> [1] "volcano"
#> 
#> $label
#> [1] "Volcano plot"
#> 
#> $run
#> function (ctx) 
#> {
#>     dea_res <- ctx$data$dea_res
#>     if (is.null(dea_res)) {
#>         cli::cli_abort(c("Missing required ctx$data for this step.", 
#>             x = "Step 'volcano' requires {.field dea_res}.", 
#>             i = "Add {.fn step_dea} before {.fn step_volcano} in the blueprint."))
#>     }
#>     p <- .run_function(glyvis::plot_volcano, dea_res, ctx$group_col, 
#>         ctx$dots)
#>     ctx_add_plot(ctx, "volcano", p, "Volcano plot for the comparison of the two groups.")
#> }
#> <bytecode: 0x559158ac7730>
#> <environment: 0x559153a7f128>
#> 
#> $report
#> function (x) 
#> {
#>     "When the comparison only contains two groups, this step will generate a volcano plot in `plots$volcano`."
#> }
#> <bytecode: 0x559158aca5a0>
#> <environment: 0x559153a7f128>
#> 
#> $outputs
#> $outputs$plots
#> [1] "volcano"
#> 
#> 
#> $require
#> [1] "dea_res"
#> 
#> $generate
#> character(0)
#> 
#> $condition
#> function (ctx) 
#> {
#>     g <- ctx$exp$sample_info[[ctx$group_col]]
#>     length(levels(g)) == 2
#> }
#> <bytecode: 0x559158ac7e30>
#> <environment: 0x559153a7f128>
#> 
#> $retry
#> [1] 0
#> 
#> attr(,"class")
#> [1] "glysmith_step"
```
