# Step: Derived Trait Calculation

Calculate glycan derived traits using
[`glydet::derive_traits()`](https://glycoverse.github.io/glydet/reference/derive_traits.html).
Advanced glycan structure analysis that summarizes structural properties
of a glycome or each glycosite. Need glycan structure information.

This step requires `exp` (experiment data).

## Usage

``` r
step_derive_traits(trait_fns = NULL, mp_fns = NULL, mp_cols = NULL)
```

## Arguments

- trait_fns:

  A named list of derived trait functions created by trait factories.
  Names of the list are the names of the derived traits. Default is
  `NULL`, which means all derived traits in
  [`traits_basic()`](https://glycoverse.github.io/glydet/reference/traits_basic.html)
  are calculated.

- mp_fns:

  A named list of meta-property functions. This parameter is useful if
  your trait functions use custom meta-properties other than those in
  [`all_mp_fns()`](https://glycoverse.github.io/glydet/reference/all_mp_fns.html).
  Default is `NULL`, which means all meta-properties in
  [`all_mp_fns()`](https://glycoverse.github.io/glydet/reference/all_mp_fns.html)
  are used.

- mp_cols:

  A character vector of column names in
  [`SummarizedExperiment::rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  to use as meta-properties. If names are provided, they will be used as
  names of the meta-properties, otherwise the column names will be used.
  When `mp_cols` is specified, the selected columns overwrite
  meta-properties introduced by `mp_fns` with the same names, including
  built-in meta-properties. Default is `NULL`, which means all columns
  in `rowData()` are available as meta-properties by their existing
  names. In this default mode, meta-properties introduced by `mp_fns`
  take precedence over `rowData()` columns with the same names.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to calculate derived traits for

Data generated:

- `trait_exp`: The experiment with derived traits

Tables generated:

- `derived_traits`: A table containing the derived traits.

## See also

[`glydet::derive_traits()`](https://glycoverse.github.io/glydet/reference/derive_traits.html)

## Examples

``` r
step_derive_traits()
#> <step "step_derive_traits()"> Derived trait calculation
```
