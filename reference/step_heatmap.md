# Step: Heatmap

Create a heatmap plot using
[`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).
The heatmap visualizes expression values across samples.

## Usage

``` r
step_heatmap(on = "exp", ...)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to plot. One of "exp",
  "sig_exp", "trait_exp", or "sig_trait_exp". Default is "exp".

- ...:

  Step-specific arguments passed to
  [`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).
  Use the format `pkg.func.arg`. For example,
  `step_heatmap(glyvis.plot_heatmap.show_rownames = TRUE)`.

## Value

A `glysmith_step` object.

## Details

Data required:

- Depends on `on` parameter (default: `exp`)

Plots generated:

- `heatmap`: A heatmap plot (if `on = "exp"`)

- `sig_heatmap`: A heatmap plot (if `on = "sig_exp"`)

- `trait_heatmap`: A heatmap plot (if `on = "trait_exp"`)

- `sig_trait_heatmap`: A heatmap plot (if `on = "sig_trait_exp"`)

## See also

[`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html)

## Examples

``` r
step_heatmap()
#> <step "step_heatmap()"> Heatmap
step_heatmap(on = "sig_exp")
#> <step "step_heatmap(on = \"sig_exp\")"> Heatmap of significant variables
step_heatmap(on = "trait_exp")
#> <step "step_heatmap(on = \"trait_exp\")"> Heatmap of traits
```
