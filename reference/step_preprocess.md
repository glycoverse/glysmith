# Step: Preprocessing

Preprocess the experiment using
[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html).
This is usually the first step, but can be omitted if the experiment is
already preprocessed.

## Usage

``` r
step_preprocess(...)
```

## Arguments

- ...:

  Step-specific arguments passed to underlying functions. Use the format
  `pkg.func.arg`. For example,
  `step_preprocess(glyclean.auto_clean.remove_preset = "discovery")`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to preprocess

Data generated:

- `raw_exp`: The raw experiment (previous `exp`, saved for reference)

This step is special in that it silently overwrites the `exp` data with
the preprocessed experiment. This ensures that no matter if
preprocessing is performed or not, the "active" experiment is always
under the key `exp`. The previous `exp` is saved as `raw_exp` for
reference.

## See also

[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html)

## Examples

``` r
step_preprocess()
#> <step "step_preprocess()"> Preprocessing
```
