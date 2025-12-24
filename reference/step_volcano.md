# Step: Volcano Plot

Create a volcano plot from DEA results using
[`glyvis::plot_volcano()`](https://glycoverse.github.io/glyvis/reference/plot_volcano.html).
This step requires one of the DEA steps to be run:

- [`step_dea_limma()`](https://glycoverse.github.io/glysmith/reference/step_dea_limma.md)

- [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/reference/step_dea_ttest.md)

- [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/reference/step_dea_wilcox.md)

## Usage

``` r
step_volcano(...)
```

## Arguments

- ...:

  Step-specific arguments passed to underlying functions. Use the format
  `pkg.func.arg`. For example,
  `step_volcano(glyvis.plot_volcano.log2fc_cutoff = 2)`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `dea_res`: The DEA results from
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)

Plots generated:

- `volcano`: A volcano plot

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glyvis.plot_volcano.log2fc_cutoff`: Log2 fold change cutoff (default:
  1).

- `glyvis.plot_volcano.p_cutoff`: P-value cutoff (default: 0.05).

- `glyvis.plot_volcano.p_col`: Column for p-value ("p_adj" or "p_val").

- `glyvis.plot_volcano.contrast`: Contrast to plot for limma results.

## See also

[`glyvis::plot_volcano()`](https://glycoverse.github.io/glyvis/reference/plot_volcano.html)

## Examples

``` r
step_volcano()
#> <step "step_volcano()"> Volcano plot
```
