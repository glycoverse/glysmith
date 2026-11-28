# Compose a tree of analytical steps

Connect a producer to its consumer with `%=>%`. Routes separated by
commas in `blueprint()` start independently from the original
experiment. Use
[`fork()`](https://glycoverse.github.io/glysmith/dev/reference/fork.md)
to share an intermediate output and
[`br()`](https://glycoverse.github.io/glysmith/dev/reference/br.md) for
a side route that leaves the outer connection available. See
[`vignette("blueprint")`](https://glycoverse.github.io/glysmith/dev/articles/blueprint.md).

## Usage

``` r
blueprint(...)

lhs %=>% rhs
```

## Arguments

- ...:

  One or more routes, optionally named. A trailing comma is allowed.

- lhs, rhs:

  Steps, routes, forks, side branches or reusable blueprints.

## Value

`blueprint()` returns a validated blueprint. Printing it draws each step
beneath its producer and shows its result ID. `%=>%` returns an ordered
route for use in a blueprint. Neither constructor runs the analysis.

## Examples

``` r
blueprint(
  step_preprocess() %=>% step_dea_limma() %=>% fork(
    step_volcano(),
    step_filter_sig_vars() %=>% step_heatmap()
  )
)
#> Blueprint (root accepts exp)
#> └─ node_1  step_preprocess() (exp)
#>   └─ node_2  step_dea_limma() (contrast)
#>     ├─ node_3  step_volcano() (terminal)
#>     └─ node_4  step_filter_sig_vars() (filtered_exp)
#>       └─ node_5  step_heatmap() (terminal)
blueprint(scores = step_pca(), differential = step_dea_limma())
#> Blueprint (root accepts exp)
#> ├─ node_1  step_pca() [scores] (terminal)
#> └─ node_2  step_dea_limma() [differential] (contrast)
```
