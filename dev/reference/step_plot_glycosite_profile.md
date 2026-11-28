# Step: Plot the glycosite profile

Plot two count distributions for the current glycoproteomics experiment
(`exp`), pooling annotations across samples. Glycomics inputs skip this
step.

## Usage

``` r
step_plot_glycosite_profile(glycan_col = NULL, plot_width = 4, plot_height = 4)
```

## Arguments

- glycan_col:

  Row-annotation column identifying glycans. With `NULL` (default),
  complete `glycan_structure` annotations are used. If structures are
  absent or incomplete, a message is printed and `glycan_composition` is
  used for the whole experiment, so structure and composition
  identifiers are never mixed. Supply a column name to select it
  explicitly.

- plot_width, plot_height:

  Export width and height of each plot in inches.

## Value

A `glysmith_step` object.

## Details

- `glycans_per_site` counts sites by the number of distinct glycans
  observed at each protein and site position, using
  [`glyvis::plot_glycans_per_site()`](https://glycoverse.github.io/glyvis/reference/plot_glycans_per_site.html).

- `sites_per_protein` counts proteins by the number of distinct
  glycosylation sites observed on each protein, using
  [`glyvis::plot_sites_per_protein()`](https://glycoverse.github.io/glyvis/reference/plot_glycans_per_site.html).

## Examples

``` r
bp <- blueprint(step_plot_glycosite_profile())
if (requireNamespace("glyvis", quietly = TRUE)) {
  result <- forge_analysis(glyexp::real_experiment, bp)
  cast_plot(result, "node_1__glycans_per_site")
  cast_plot(result, "node_1__sites_per_protein")
}
```
