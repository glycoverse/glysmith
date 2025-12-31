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
  "sig_exp", "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".
  Default is "exp".

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

- `motif_heatmap`: A heatmap plot (if `on = "motif_exp"`)

- `sig_motif_heatmap`: A heatmap plot (if `on = "sig_motif_exp"`)

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glyvis.plot_heatmap.show_rownames`: Whether to show row names.

- `glyvis.plot_heatmap.show_colnames`: Whether to show column names.

- `glyvis.plot_heatmap.cluster_rows`: Whether to cluster rows.

- `glyvis.plot_heatmap.cluster_cols`: Whether to cluster columns.

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
