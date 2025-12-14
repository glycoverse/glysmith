# Step: Differential Expression Analysis (DEA)

Run differential analysis using
[`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).

## Usage

``` r
step_dea()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_dea()
#> $id
#> [1] "dea"
#> 
#> $label
#> [1] "Differential expression analysis"
#> 
#> $run
#> function (ctx) 
#> {
#>     dea_res <- .run_function(glystats::gly_limma, ctx$exp, ctx$group_col, 
#>         ctx$dots, "group_col")
#>     ctx$data$dea_res <- dea_res
#>     ctx_add_table(ctx, "dea", glystats::get_tidy_result(dea_res), 
#>         "Differential expression analysis results of all comparisons for all variables.")
#> }
#> <bytecode: 0x55ad9cec51b0>
#> <environment: 0x55ada1bb2dc0>
#> 
#> $report
#> function (x) 
#> {
#>     tbl <- x$tables[["dea"]]
#>     sig <- length(unique(tbl$variable[tbl$p_adj < 0.05]))
#>     msg <- "Differential expression analysis was performed and the results were saved in `tables$dea`. "
#>     if (sig > 0) {
#>         msg <- paste0(msg, "Number of significant items (FDR/adjusted p < 0.05): ", 
#>             sig, ".\n")
#>     }
#>     else {
#>         msg <- paste0(msg, "No significant items (FDR/adjusted p < 0.05). \n")
#>     }
#>     msg
#> }
#> <bytecode: 0x55ad9cec7b88>
#> <environment: 0x55ada1bb2dc0>
#> 
#> $outputs
#> $outputs$tables
#> [1] "dea"
#> 
#> 
#> $require
#> character(0)
#> 
#> $generate
#> [1] "dea_res"
#> 
#> $condition
#> NULL
#> 
#> $retry
#> [1] 0
#> 
#> attr(,"class")
#> [1] "glysmith_step"
```
