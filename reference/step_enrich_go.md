# Step: GO Enrichment Analysis

Perform GO enrichment analysis on differentially expressed variables
using
[`glystats::gly_enrich_go()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).
This step requires
[`step_dea()`](https://glycoverse.github.io/glysmith/reference/step_dea.md).

## Usage

``` r
step_enrich_go()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_enrich_go()
#> $id
#> [1] "enrich_go"
#> 
#> $label
#> [1] "GO enrichment analysis"
#> 
#> $run
#> function (ctx) 
#> {
#>     sig_exp <- glystats::filter_sig_vars(ctx$exp, ctx$data$dea_res)
#>     enrich_res <- .run_function(f, ctx$exp, ctx$group_col, ctx$dots)
#>     ctx <- ctx_add_table(ctx, kind, glystats::get_tidy_result(enrich_res), 
#>         paste0(toupper(kind), " enrichment analysis results."))
#>     p <- .run_function(glyvis::plot_enrich, enrich_res, ctx$group_col, 
#>         ctx$dots)
#>     ctx_add_plot(ctx, kind, p, paste0(toupper(kind), " enrichment analysis plot."))
#> }
#> <bytecode: 0x55ada35ddca0>
#> <environment: 0x55ada06f3d58>
#> 
#> $report
#> function (x) 
#> {
#>     tbl <- x$tables[[kind]]
#>     n_sig <- sum(tbl$p_adj < 0.05)
#>     msg <- paste0("Enrichment analysis was performed on differentially expressed variables and the results were saved in `tables$", 
#>         kind, "` and `plots$", kind, "`.")
#>     if (n_sig > 0) {
#>         msg <- paste0(msg, " Number of significant items (FDR/adjusted p < 0.05): ", 
#>             n_sig, ".\n\n")
#>         msg <- paste0(msg, "Top terms: \n\n", paste("- ", tbl$description[1:min(5, 
#>             n_sig)], collapse = "\n"), "\n")
#>     }
#>     else {
#>         msg <- paste0(msg, " No significant items (FDR/adjusted p < 0.05).\n")
#>     }
#>     msg
#> }
#> <bytecode: 0x55ada35e1d48>
#> <environment: 0x55ada06f3d58>
#> 
#> $outputs
#> $outputs$tables
#> [1] "go"
#> 
#> $outputs$plots
#> [1] "go"
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
#> glyexp::get_exp_type(ctx$exp) == "glycoproteomics"
#> <bytecode: 0x55ada35de090>
#> <environment: 0x55ada06f3d58>
#> 
#> $retry
#> [1] 0
#> 
#> attr(,"class")
#> [1] "glysmith_step"
```
