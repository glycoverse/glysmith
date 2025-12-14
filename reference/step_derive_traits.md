# Step: Derived Trait Calculation

Calculate glycan derived traits using
[`glydet::derive_traits()`](https://glycoverse.github.io/glydet/reference/derive_traits.html).

## Usage

``` r
step_derive_traits()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_derive_traits()
#> $id
#> [1] "derive_traits"
#> 
#> $label
#> [1] "Derived trait calculation"
#> 
#> $run
#> function (ctx) 
#> {
#>     trait_exp <- .run_function(glydet::derive_traits, ctx$exp, 
#>         ctx$group_col, ctx$dots)
#>     ctx$data$trait_exp <- trait_exp
#>     ctx_add_table(ctx, "derived_traits", tibble::as_tibble(trait_exp), 
#>         "Derived trait calculation results.")
#> }
#> <bytecode: 0x55ada3601860>
#> <environment: 0x55ada1175410>
#> 
#> $report
#> function (x) 
#> {
#>     tbl <- x$tables[["derived_traits"]]
#>     if (glyexp::get_exp_type(x$exp) == "glycomics") {
#>         item_name <- "Derived traits"
#>     }
#>     else {
#>         item_name <- "Site-specific derived traits"
#>     }
#>     paste0("Derived traits were calculated and the results were saved in `tables$derived_traits`. ", 
#>         "Number of derived traits: ", length(unique(tbl$trait)), 
#>         ".")
#> }
#> <bytecode: 0x55ada36044d8>
#> <environment: 0x55ada1175410>
#> 
#> $outputs
#> $outputs$tables
#> [1] "derived_traits"
#> 
#> 
#> $require
#> character(0)
#> 
#> $generate
#> [1] "trait_exp"
#> 
#> $condition
#> function (ctx) 
#> "glycan_structure" %in% colnames(ctx$exp$var_info)
#> <bytecode: 0x55ada3601cc0>
#> <environment: 0x55ada1175410>
#> 
#> $retry
#> [1] 0
#> 
#> attr(,"class")
#> [1] "glysmith_step"
```
