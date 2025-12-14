# Step: Differential Trait Analysis (DTA)

Run differential analysis on derived traits using
[`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).
This step requires
[`step_derive_traits()`](https://glycoverse.github.io/glysmith/reference/step_derive_traits.md).

## Usage

``` r
step_dta()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_dta()
#> $id
#> [1] "dta"
#> 
#> $label
#> [1] "Differential trait analysis"
#> 
#> $run
#> function (ctx) 
#> {
#>     trait_exp <- ctx$data$trait_exp
#>     if (is.null(trait_exp)) {
#>         cli::cli_abort(c("Missing required ctx$data for this step.", 
#>             x = "Step 'dta' requires {.field trait_exp}.", i = "Add {.fn step_derive_traits} before {.fn step_dta} in the blueprint."))
#>     }
#>     filtered_trait_exp <- glyclean::remove_constant(trait_exp)
#>     dta_res <- .run_function(glystats::gly_limma, filtered_trait_exp, 
#>         ctx$group_col, ctx$dots, "group_col")
#>     ctx_add_table(ctx, "dta", glystats::get_tidy_result(dta_res), 
#>         "Differential trait analysis results.")
#> }
#> <bytecode: 0x559158b13cf8>
#> <environment: 0x5591594f6770>
#> 
#> $report
#> function (x) 
#> {
#>     tbl <- x$tables[["dta"]]
#>     sig <- length(unique(tbl$variable[tbl$p_adj < 0.05]))
#>     msg <- "Differential trait analysis was performed and the results were saved in `tables$dta`. "
#>     if (glyexp::get_exp_type(x$exp) == "glycomics") {
#>         item_name <- "traits"
#>     }
#>     else {
#>         item_name <- "site-specific traits"
#>     }
#>     if (sig > 0) {
#>         msg <- paste0(msg, "Number of significant ", item_name, 
#>             " (FDR/adjusted p < 0.05): ", sig, ".\n")
#>     }
#>     else {
#>         msg <- paste0(msg, "No significant ", item_name, " (FDR/adjusted p < 0.05).\n")
#>     }
#>     msg
#> }
#> <bytecode: 0x559158b12910>
#> <environment: 0x5591594f6770>
#> 
#> $outputs
#> $outputs$tables
#> [1] "dta"
#> 
#> 
#> $require
#> [1] "trait_exp"
#> 
#> $generate
#> character(0)
#> 
#> $condition
#> function (ctx) 
#> "glycan_structure" %in% colnames(ctx$exp$var_info)
#> <bytecode: 0x559158b14158>
#> <environment: 0x5591594f6770>
#> 
#> $retry
#> [1] 0
#> 
#> attr(,"class")
#> [1] "glysmith_step"
```
