# Changelog

## glysmith (development version)

## glysmith 0.3.1

### Minor improvements and bug fixes

- Fix deployment issue on r-universe.

## glysmith 0.3.0

### New features

- Added
  [`step_quantify_motifs()`](https://glycoverse.github.io/glysmith/reference/step_quantify_motifs.md)
  for quantifying glycan motifs. Updated relevant steps to support the
  `on` parameter for `motif_exp` experiments.
- [`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
  now prints an LLM-generated explanation to the console before
  returning the blueprint.
- [`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
  now supports an `exp` argument, accepting a
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object, to provide more accurate, context-aware suggestions.

### Minor improvements and bug fixes

- Refined step documentation to improve clarity and optimize
  [`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
  performance.
- Enhanced error messages for failed steps.
- Suppressed a harmless warning in
  [`quench_result()`](https://glycoverse.github.io/glysmith/reference/quench_result.md)
  when used with
  [`step_pca()`](https://glycoverse.github.io/glysmith/reference/step_pca.md).
- Console messages from
  [`step_pca()`](https://glycoverse.github.io/glysmith/reference/step_pca.md)
  now include the experiment name during
  [`forge_analysis()`](https://glycoverse.github.io/glysmith/reference/forge_analysis.md).
- Moved several dependencies (`pROC`, `Rtsne`, `uwot`,
  `EnhancedVolcano`, `org.Hs.eg.db`, `clusterProfiler`, `ggplotify`,
  `pheatmap`, and `factoextra`) to “Suggests”. These are now
  automatically checked and installed when required by blueprint steps.

## glysmith 0.2.0

### New features

- Add
  [`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
  use a Large Language Model to generate a blueprint from user prompt.
- Add
  [`step_tsne()`](https://glycoverse.github.io/glysmith/reference/step_tsne.md)
  for t-distributed Stochastic Neighbor Embedding.
- Add
  [`step_umap()`](https://glycoverse.github.io/glysmith/reference/step_umap.md)
  for Uniform Manifold Approximation and Projection.
- Add
  [`step_roc()`](https://glycoverse.github.io/glysmith/reference/step_roc.md)
  for Receiver Operating Characteristic analysis.
- [`step_pca()`](https://glycoverse.github.io/glysmith/reference/step_pca.md)
  now also generates loading plot and screeplot, besides the individual
  plot.
- [`step_pca()`](https://glycoverse.github.io/glysmith/reference/step_pca.md)
  (along with the newly added
  [`step_tsne()`](https://glycoverse.github.io/glysmith/reference/step_tsne.md)
  and
  [`step_umap()`](https://glycoverse.github.io/glysmith/reference/step_umap.md))
  now has an `on` parameter to control which experiment to perform the
  analysis on.

### Minor improvements and bug fixes

- Update report content of
  [`step_ident_overview()`](https://glycoverse.github.io/glysmith/reference/step_ident_overview.md).
  This is to adapt to the new
  [`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/summarize_experiment.html)
  function introduced in `glyexp` 0.11.0.
- Update
  [`blueprint_default()`](https://glycoverse.github.io/glysmith/reference/blueprint_default.md):
  [`step_ident_overview()`](https://glycoverse.github.io/glysmith/reference/step_ident_overview.md)
  is now the first step, before
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md).
- Fix the bug that report content of DEA step for derived traits has “NA
  vs NA” contrast.
- Some plot names, table names, or step IDs have been changed to be more
  consistent.
- Update documentations of some step functions to better reflect their
  usage.
- `glysmith_result` now has a `data` field to store the data generated
  by the blueprint.

## glysmith 0.1.0

- First GitHub release.
