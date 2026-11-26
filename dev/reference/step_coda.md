# Compositional data transformation

Transform the selected experiment to centered log-ratio (CLR) or
additive log-ratio (ALR) coordinates. By default,
[`glyclean::auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.html)
chooses the transformation.

## Usage

``` r
step_coda(
  method = "auto",
  by = NULL,
  gamma = 0.1,
  group_scales = NULL,
  seed = 123,
  on = "exp"
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

- on:

  Name of the experiment in the blueprint. Defaults to `"exp"`. For
  example, use `"dynamic_motif_exp"` or `"branch_motif_exp"` after
  quantifying motifs, then use the same `on` in the statistical step.

## Value

A `glysmith_step` object.

## Details

Place this step after abundance preprocessing and before statistical
analysis. The input must contain finite, positive abundances on the raw
scale or finite values on a known log scale. A transformation error
stops the blueprint so subsequent tests cannot accidentally use the
input scale.

The step replaces `on` with the transformed experiment and saves its
previous value in `pre_coda_<on>` (by default, `pre_coda_exp`).
Subsequent steps targeting the same experiment use its log2-ratio
coordinates directly. A successful ALR omits the reference feature from
those tests. Use
[`glyclean::get_pipeline()`](https://glycoverse.github.io/glyclean/reference/glyclean_pipeline.html)
on the output to see which transform ran.

Differential tests return p-values on these coordinates. For example,
limma's `estimate` is the contrast in log2-ratio units. Abundance
`log2fc` is undefined, so significant features are selected by p-value
([`glystats::filter_sig_vars()`](https://glycoverse.github.io/glystats/reference/filter_sig_vars.html)
ignores an explicit fold-change cutoff with a warning). Volcano plots
and fold-change-based protein enrichment are skipped. Run abundance QC,
trait derivation, and motif quantification before transforming the main
experiment.

## Examples

``` r
blueprint(step_coda(by = "group", gamma = 0), step_pca(), step_dea_limma())
#> 
#> ── Blueprint (3 steps) ──
#> 
#> • step_coda(by = "group", gamma = 0)
#> • step_pca()
#> • step_dea_limma()
step_coda(method = "clr", gamma = 0)
#> <step "step_coda(method = \"clr\", gamma = 0)"> Compositional data
#> transformation
blueprint(
  step_quantify_branch_motifs(),
  step_coda(on = "branch_motif_exp"),
  step_dea_limma(on = "branch_motif_exp")
)
#> 
#> ── Blueprint (3 steps) ──
#> 
#> • step_quantify_branch_motifs()
#> • step_coda(on = "branch_motif_exp")
#> • step_dea_limma(on = "branch_motif_exp")
```
