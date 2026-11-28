# Run a single side route

The branch starts at the current output. The next outer step also
receives that output. Use
[`fork()`](https://glycoverse.github.io/glysmith/dev/reference/fork.md)
to end a route with multiple siblings.

## Usage

``` r
br(name, route)
```

## Arguments

- name:

  A label for the branch.

- route:

  One step, sequence, fork or reusable blueprint.

## Value

A branch for use inside
[`blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/blueprint.md).

## Examples

``` r
blueprint(br("scores", step_pca()) %=>% step_dea_limma())
#> Blueprint (root accepts exp)
#> ├─ node_1  step_pca() [scores] (terminal)
#> └─ node_2  step_dea_limma() (contrast)
```
