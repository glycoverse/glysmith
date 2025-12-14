# Step: Identification Overview

Summarize the experiment using
[`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/count_compositions.html).

## Usage

``` r
step_ident_overview()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_ident_overview()
#> $id
#> [1] "identification_overview"
#> 
#> $label
#> [1] "Identification overview"
#> 
#> $run
#> function (ctx) 
#> {
#>     tbl <- .run_function(glyexp::summarize_experiment, ctx$exp, 
#>         ctx$group_col, ctx$dots)
#>     ctx_add_table(ctx, "summary", tbl, "Identification overview of the experiment.")
#> }
#> <bytecode: 0x559158ab7328>
#> <environment: 0x559155a5dde8>
#> 
#> $report
#> function (x) 
#> {
#>     tbl <- x$tables[["summary"]]
#>     parts <- paste0(tbl$n, " ", tbl$item, "s")
#>     paste0("In total, ", glue::glue_collapse(parts, sep = ", ", 
#>         last = ", and "), " were identified.")
#> }
#> <bytecode: 0x559158aba780>
#> <environment: 0x559155a5dde8>
#> 
#> $outputs
#> $outputs$tables
#> [1] "summary"
#> 
#> 
#> $require
#> character(0)
#> 
#> $generate
#> character(0)
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
