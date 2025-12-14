# Step: Preprocessing

Preprocess the experiment using
[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html).

## Usage

``` r
step_preprocess()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_preprocess()
#> $id
#> [1] "preprocessing"
#> 
#> $label
#> [1] "Preprocessing"
#> 
#> $run
#> function (ctx) 
#> {
#>     ctx$exp <- .run_function(glyclean::auto_clean, ctx$exp, ctx$group_col, 
#>         ctx$dots, "group_col")
#>     ctx
#> }
#> <bytecode: 0x55ad9ce91cb8>
#> <environment: 0x55ada0966248>
#> 
#> $report
#> NULL
#> 
#> $outputs
#> list()
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
