# Step: Derived Trait Calculation

Calculate glycan derived traits using
[`glydet::derive_traits()`](https://glycoverse.github.io/glydet/reference/derive_traits.html).

## Usage

``` r
step_derive_traits(...)
```

## Arguments

- ...:

  Step-specific arguments passed to underlying functions. Use the format
  `pkg.func.arg`.

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
