# Compositional data transformation

Analyze the experiment supplied by the upstream step.

## Usage

``` r
step_coda(
  method = "auto",
  by = NULL,
  gamma = 0.1,
  group_scales = NULL,
  seed = 123
)
```

## Arguments

- method:

  Use `"auto"` (default) for
  [`glyclean::auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.html),
  `"clr"` for
  [`glyclean::transform_clr()`](https://glycoverse.github.io/glyclean/reference/transform_clr.html),
  or `"alr"` for
  [`glyclean::transform_alr()`](https://glycoverse.github.io/glyclean/reference/transform_alr.html).
  Automatic selection uses site-wise CLR for glycoproteomics. For other
  experiments, it uses CLR for up to 50 features and attempts ALR
  above 50. The ALR method falls back to CLR if no suitable reference is
  available. Glycoproteomics supports `"auto"` and `"clr"`.

- by:

  Either a column name in `sample_info` or a factor/vector with one
  value per sample.

- gamma:

  Standard deviation of the scale-uncertainty model on the `log2` scale.
  Default is `0.1`. Set to `0` for deterministic transformation.

- group_scales:

  Optional informed group scales. For binary comparisons, this can be a
  single positive ratio for the second group relative to the first, or
  two positive scales from which that ratio is derived. For multi-group
  data, provide a positive vector with one scale per group. All scales
  and any derived two-group ratio must be finite and strictly positive.

- seed:

  Non-negative integer seed for random number generation. Default is
  `123`. The caller's random state is restored, including on errors.

## Value

A `glysmith_step` object.

## Examples

``` r
blueprint(step_coda(by = "group", gamma = 0) %=>% fork(step_pca(), step_dea_limma()))
#> Blueprint (root accepts exp)
#> └─ node_1  step_coda(by = "group", gamma = 0) (exp)
#>   ├─ node_2  step_pca() (terminal)
#>   └─ node_3  step_dea_limma() (contrast)
step_coda(method = "clr", gamma = 0)
#> <step "step_coda(method = \"clr\", gamma = 0)"> Compositional data
#> transformation
blueprint(
  step_quantify_branch_motifs() %=>% step_coda() %=>% step_dea_limma()
)
#> Blueprint (root accepts exp)
#> └─ node_1  step_quantify_branch_motifs() (exp)
#>   └─ node_2  step_coda() (exp)
#>     └─ node_3  step_dea_limma() (contrast)
```
