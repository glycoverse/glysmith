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
  [`basic_traits()`](https://glycoverse.github.io/glydet/reference/basic_traits.html)
  are calculated.

- mp_fns:

  A named list of meta-property functions. This parameter is useful if
  your trait functions use custom meta-properties other than those in
  [`all_mp_fns()`](https://glycoverse.github.io/glydet/reference/all_mp_fns.html).
  Default is `NULL`, which means all meta-properties in
  [`all_mp_fns()`](https://glycoverse.github.io/glydet/reference/all_mp_fns.html)
  are used.

- mp_cols:

  A character vector of column names in the `var_info` tibble to use as
  meta-properties. If names are provided, they will be used as names of
  the meta-properties, otherwise the column names will be used.
  Meta-properties specified in `mp_cols` will overwrite those introduced
  by `mp_fns` with the same names, including the built-in
  meta-properties. Default is `NULL`, which means no columns are used as
  meta-properties.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to calculate derived traits for

Data generated:

- `trait_exp`: The experiment with derived traits

Tables generated:

- `derived_traits`: A table containing the derived traits.

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Include this step by default if the experiment has glycan structures.

- After this step, it should be followed by the DEA and visualization
  steps.

## See also

[`glydet::derive_traits()`](https://glycoverse.github.io/glydet/reference/derive_traits.html)

## Examples

``` r
step_derive_traits()
#> <step "step_derive_traits()"> Derived trait calculation
```
