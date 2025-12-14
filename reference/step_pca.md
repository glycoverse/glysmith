# Step: Principal Component Analysis (PCA)

Run PCA using
[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html)
and plot it with
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html).

## Usage

``` r
step_pca()
```

## Value

A `glysmith_step` object.

## Examples

``` r
step_pca()
#> $id
#> [1] "pca"
#> 
#> $label
#> [1] "Principal component analysis"
#> 
#> $run
#> function (ctx) 
#> {
#>     pca_res <- .run_function(glystats::gly_pca, ctx$exp, ctx$group_col, 
#>         ctx$dots)
#>     ctx <- ctx_add_table(ctx, "pca_samples", glystats::get_tidy_result(pca_res, 
#>         "samples"), "PCA scores for each sample.")
#>     ctx <- ctx_add_table(ctx, "pca_variables", glystats::get_tidy_result(pca_res, 
#>         "variables"), "PCA loadings for each variable.")
#>     ctx <- ctx_add_table(ctx, "pca_eigenvalues", glystats::get_tidy_result(pca_res, 
#>         "eigenvalues"), "PCA eigenvalues.")
#>     p <- .run_function(glyvis::plot_pca, pca_res, ctx$group_col, 
#>         ctx$dots, "group_col")
#>     ctx_add_plot(ctx, "pca", p, "PCA plot colored by group.")
#> }
#> <bytecode: 0x5591523b7d80>
#> <environment: 0x559159553708>
#> 
#> $report
#> function (x) 
#> {
#>     eig <- x$tables[["pca_eigenvalues"]]
#>     "PCA was performed and the results were saved in `plots$pca` and `tables$pca_*`."
#> }
#> <bytecode: 0x5591523bb728>
#> <environment: 0x559159553708>
#> 
#> $outputs
#> $outputs$tables
#> [1] "pca_samples"     "pca_variables"   "pca_eigenvalues"
#> 
#> $outputs$plots
#> [1] "pca"
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
