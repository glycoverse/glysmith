# Custom Blueprint

``` r

library(glysmith)
```

A blueprint is simply an analytical plan that tells
[`forge_analysis()`](https://glycoverse.github.io/glysmith/dev/reference/forge_analysis.md)
what to do. By default,
[`forge_analysis()`](https://glycoverse.github.io/glysmith/dev/reference/forge_analysis.md)
runs a default blueprint:

``` r

blueprint_default()
#> Blueprint (root accepts exp)
#> ├─ node_1  step_ident_overview() [overview] (terminal)
#> └─ node_2  step_preprocess() (exp)
#>   ├─ node_3  step_plot_qc(when = "post") [qc] (terminal)
#>   ├─ node_4  step_pca() [scores] (terminal)
#>   ├─ node_5  step_dea_limma() [abundances] (contrast)
#>   │ ├─ node_6  step_volcano() [abundances] (terminal)
#>   │ ├─ node_7  step_filter_sig_vars() [abundances] (filtered_exp)
#>   │ │ └─ node_8  step_heatmap() [abundances] (terminal)
#>   │ ├─ node_9  step_sig_enrich_go() [abundances] (terminal)
#>   │ ├─ node_10  step_sig_enrich_kegg() [abundances] (terminal)
#>   │ └─ node_11  step_sig_enrich_reactome() [abundances] (terminal)
#>   └─ node_12  step_derive_traits() [traits] (exp)
#>     └─ node_13  step_dea_limma() [traits] (contrast)
#>       └─ node_14  step_filter_sig_vars() [traits] (filtered_exp)
#>         └─ node_15  step_heatmap() [traits] (terminal)
```

This vignette shows you how to build your own blueprint for custom
analyses.

**Heads up:** Before diving into the details of manual blueprint
creation, you might want to try having an LLM generate one for you with
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md).
Check out [this
vignette](https://glycoverse.github.io/glysmith/articles/ai.html) for
the details.

## Connect steps by their outputs

Use `%=>%` to pass the output of one step to the next. Each
comma-separated argument to
[`blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/blueprint.md)
starts a route on the original experiment. Glysmith reserves `%=>%` for
blueprint composition. Ordinary R and magrittr data pipelines keep their
own operators. The following routes each fit one model and give its
result to two consumers. Use a two-group experiment for the t-test.

``` r

dea_bp1 <- blueprint(
  step_dea_limma() %=>% fork(
    step_volcano(),
    step_filter_sig_vars() %=>% step_heatmap()
  )
)
dea_bp2 <- blueprint(
  step_dea_ttest() %=>% fork(
    step_volcano(),
    step_filter_sig_vars() %=>% step_heatmap()
  )
)

# Two independent entry routes run preprocessing twice.
independent <- blueprint(
  limma = step_preprocess() %=>% dea_bp1,
  ttest = step_preprocess() %=>% dea_bp2,
)

# One preprocessing step supplies both saved analysis routes.
shared <- blueprint(step_preprocess() %=>% fork(limma = dea_bp1, ttest = dea_bp2))
print(shared)
#> Blueprint (root accepts exp)
#> └─ node_1  step_preprocess() (exp)
#>   ├─ node_2  step_dea_limma() [limma] (contrast)
#>   │ ├─ node_3  step_volcano() [limma] (terminal)
#>   │ └─ node_4  step_filter_sig_vars() [limma] (filtered_exp)
#>   │   └─ node_5  step_heatmap() [limma] (terminal)
#>   └─ node_6  step_dea_ttest() [ttest] (contrast)
#>     ├─ node_7  step_volcano() [ttest] (terminal)
#>     └─ node_8  step_filter_sig_vars() [ttest] (filtered_exp)
#>       └─ node_9  step_heatmap() [ttest] (terminal)
```

Each argument of
[`fork()`](https://glycoverse.github.io/glysmith/dev/reference/fork.md)
receives the current output. A fork can be nested or start at the inlet,
and ends its route. A side route preserves that output for the next
step. For example, `br("scores", step_pca()) %=>% step_dea_limma()`
supplies the same experiment to PCA and limma.

This blueprint derives traits once and fits ANOVA to both the abundance
and trait experiments. PCA receives the same traits as the second ANOVA.

``` r

inspect_fit <- blueprint(
  step_dea_anova() %=>% fork(
    strict = step_filter_sig_vars(0.01),
    selected = step_filter_sig_vars() %=>% step_heatmap()
  )
)
traits <- step_derive_traits() %=>% fork(step_pca(), inspect_fit)
bp <- blueprint(step_preprocess() %=>% fork(abundances = inspect_fit, traits = traits))
print(bp)
#> Blueprint (root accepts exp)
#> └─ node_1  step_preprocess() (exp)
#>   ├─ node_2  step_dea_anova() [abundances] (omnibus)
#>   │ ├─ node_3  step_filter_sig_vars(p_adj_cutoff = 0.01) [abundances/strict] (filtered_exp)
#>   │ └─ node_4  step_filter_sig_vars() [abundances/selected] (filtered_exp)
#>   │   └─ node_5  step_heatmap() [abundances/selected] (terminal)
#>   └─ node_6  step_derive_traits() [traits] (exp)
#>     ├─ node_7  step_pca() [traits] (terminal)
#>     └─ node_8  step_dea_anova() [traits] (omnibus)
#>       ├─ node_9  step_filter_sig_vars(p_adj_cutoff = 0.01) [traits/strict] (filtered_exp)
#>       └─ node_10  step_filter_sig_vars() [traits/selected] (filtered_exp)
#>         └─ node_11  step_heatmap() [traits/selected] (terminal)
```

| Step | Input | Main output |
|:---|:---|:---|
| Preprocessing, group subsetting, structure inference, traits, motifs | Experiment | Transformed experiment |
| Limma, t-test, Wilcoxon | Experiment | Contrast fit |
| ANOVA, Kruskal–Wallis | Experiment | Omnibus fit |
| [`step_filter_sig_vars()`](https://glycoverse.github.io/glysmith/dev/reference/step_filter_sig_vars.md) | Either fit | Filtered experiment |
| [`step_volcano()`](https://glycoverse.github.io/glysmith/dev/reference/step_volcano.md) | Contrast fit with abundance fold changes | Terminal plot |
| Enrichment | Fit and its bound experiment | Terminal analysis |
| [`step_sig_boxplot()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_boxplot.md) | Filtered experiment from [`step_filter_sig_vars()`](https://glycoverse.github.io/glysmith/dev/reference/step_filter_sig_vars.md) | Terminal plot |
| QC, PCA and other dimension reduction, heatmap, ROC, correlation, Cox and other summaries | Experiment (including filtered) | Terminal analysis |

DEA steps fit the experiment on their incoming wire. Set cutoffs on
[`step_filter_sig_vars()`](https://glycoverse.github.io/glysmith/dev/reference/step_filter_sig_vars.md).
A fit’s `filter_*` arguments supply defaults that each filter can
override. To compare thresholds, use
`blueprint(step_dea_anova() %=>% fork(step_filter_sig_vars(0.01), step_filter_sig_vars(0.05)))`.
Each filter uses the exact experiment that produced the fit. A filter
that selects zero variables succeeds and returns a zero-row experiment.
Downstream analyses skip an empty input. A skipped or failed step blocks
its descendants, and independent branches continue.

``` r

result <- forge_analysis(glyexp::real_experiment2, bp)
result$meta$nodes        # step, producer, route, status and reason
cast_data(result)        # node outputs and auxiliary objects
cast_plot(result)        # e.g. node_5__heatmap
cast_table(result)
glyvis::autoplot(cast_data(result, "node_2"))  # abundance ANOVA main test
quench_result(result, "connected-results")
polish_report(result, "connected-report.html", open = FALSE)
```

Use the printed node ID to retrieve its main output, e.g.
`cast_data(result, "node_2")`. Plot, table and auxiliary object names
start with the producing node ID and two underscores. The prepared input
is available as `cast_data(result, "root")`. For each fit, its parent ID
retrieves the experiment used to fit the model.
[`cast_exp()`](https://glycoverse.github.io/glysmith/dev/reference/cast_exp.md)
returns the last experiment on the main route, or the input if that
route only fits or plots. With multiple top-level routes,
[`cast_exp()`](https://glycoverse.github.io/glysmith/dev/reference/cast_exp.md)
returns the original input. If the single main-route producer failed or
was skipped, the main experiment is `NULL`. Significant-variable
boxplots with more than 25 retained rows require unique fitted variable
names for ranking.

## Available steps

Here’s the full lineup of available step functions:

| Function | Description | Require Glycan Structure | Experiment Type |
|:---|:---|:--:|:--:|
| **Preprocessing** |  |  |  |
| [`step_ident_overview()`](https://glycoverse.github.io/glysmith/dev/reference/step_ident_overview.md) | Summarize the experiment (identification overview) | No | GP/G |
| [`step_plot_qc()`](https://glycoverse.github.io/glysmith/dev/reference/step_plot_qc.md) | Generate quality control plots | No | GP/G |
| [`step_preprocess()`](https://glycoverse.github.io/glysmith/dev/reference/step_preprocess.md) | Preprocess the experiment (normalization, imputation, etc.) | No | GP/G |
| [`step_zero_to_na()`](https://glycoverse.github.io/glysmith/dev/reference/step_zero_to_na.md) | Convert zeros to missing measurements | No | GP/G |
| [`step_remove()`](https://glycoverse.github.io/glysmith/dev/reference/step_remove.md) | Filter features by missingness, abundance, or variation | No | GP/G |
| [`step_normalize()`](https://glycoverse.github.io/glysmith/dev/reference/step_normalize.md) | Normalize sample abundances | No | GP/G |
| [`step_impute()`](https://glycoverse.github.io/glysmith/dev/reference/step_impute.md) | Impute missing abundances | No | GP/G |
| [`step_aggregate()`](https://glycoverse.github.io/glysmith/dev/reference/step_aggregate.md) | Combine measurements at a glycan, glycoform, or glycopeptide level | Depends on level | GP/G |
| [`step_detect_batch_effect()`](https://glycoverse.github.io/glysmith/dev/reference/step_detect_batch_effect.md) | Save a table of batch effect p-values by feature | No | GP/G |
| [`step_correct_batch_effect()`](https://glycoverse.github.io/glysmith/dev/reference/step_correct_batch_effect.md) | Correct batch effects automatically or with ComBat or limma | No | GP/G |
| [`step_transform_log()`](https://glycoverse.github.io/glysmith/dev/reference/step_transform_log.md) | Store abundances on a logarithmic scale | No | GP/G |
| [`step_coda()`](https://glycoverse.github.io/glysmith/dev/reference/step_coda.md) | Transform abundances to log-ratio coordinates for compositional data analysis | No | GP/G |
| [`step_subset_groups()`](https://glycoverse.github.io/glysmith/dev/reference/step_subset_groups.md) | Subset the experiment to specific groups | No | GP/G |
| [`step_adjust_protein()`](https://glycoverse.github.io/glysmith/dev/reference/step_adjust_protein.md) | Adjust glycoform quantification by protein abundance | No | GP |
| **Differential Analysis** |  |  |  |
| [`step_dea_limma()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_limma.md) | Differential expression analysis using limma | No | GP/G |
| [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_ttest.md) | Differential expression analysis using t-test | No | GP/G |
| [`step_dea_anova()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_anova.md) | Differential expression analysis using ANOVA | No | GP/G |
| [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_wilcox.md) | Differential expression analysis using Wilcoxon test | No | GP/G |
| [`step_dea_kruskal()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_kruskal.md) | Differential expression analysis using Kruskal-Wallis test | No | GP/G |
| [`step_filter_sig_vars()`](https://glycoverse.github.io/glysmith/dev/reference/step_filter_sig_vars.md) | Filter the fitted experiment by significance | No | GP/G |
| **Visualization** |  |  |  |
| [`step_volcano()`](https://glycoverse.github.io/glysmith/dev/reference/step_volcano.md) | Create volcano plot from DEA results | No | GP/G |
| [`step_heatmap()`](https://glycoverse.github.io/glysmith/dev/reference/step_heatmap.md) | Create heatmap plot | No | GP/G |
| [`step_logo()`](https://glycoverse.github.io/glysmith/dev/reference/step_logo.md) | Create logo plot for glycosylation sites | No | GP |
| [`step_plot_glycosite_profile()`](https://glycoverse.github.io/glysmith/dev/reference/step_plot_glycosite_profile.md) | Plot distinct glycans per site and glycosylation sites per protein | No | GP |
| [`step_sig_boxplot()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_boxplot.md) | Boxplots for significant variables from DEA | No | GP/G |
| **Dimension Reduction** |  |  |  |
| [`step_pca()`](https://glycoverse.github.io/glysmith/dev/reference/step_pca.md) | Principal Component Analysis | No | GP/G |
| [`step_tsne()`](https://glycoverse.github.io/glysmith/dev/reference/step_tsne.md) | t-SNE analysis | No | GP/G |
| [`step_umap()`](https://glycoverse.github.io/glysmith/dev/reference/step_umap.md) | UMAP analysis | No | GP/G |
| [`step_plsda()`](https://glycoverse.github.io/glysmith/dev/reference/step_plsda.md) | Partial Least Squares Discriminant Analysis | No | GP/G |
| [`step_oplsda()`](https://glycoverse.github.io/glysmith/dev/reference/step_oplsda.md) | Orthogonal Partial Least Squares Discriminant Analysis | No | GP/G |
| **Correlation & Enrichment** |  |  |  |
| [`step_correlation()`](https://glycoverse.github.io/glysmith/dev/reference/step_correlation.md) | Pairwise correlation analysis | No | GP/G |
| [`step_sig_enrich_go()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_go.md) | GO enrichment analysis on DE variables | No | GP |
| [`step_sig_enrich_kegg()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_kegg.md) | KEGG enrichment analysis on DE variables | No | GP |
| [`step_sig_enrich_reactome()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_reactome.md) | Reactome enrichment analysis on DE variables | No | GP |
| [`step_sig_enrich_ncg()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_ncg.md) | NCG enrichment analysis on DE variables | No | GP |
| [`step_sig_enrich_wp()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_wp.md) | WikiPathways enrichment analysis on DE variables | No | GP |
| [`step_sig_enrich_do()`](https://glycoverse.github.io/glysmith/dev/reference/step_sig_enrich_do.md) | Disease Ontology enrichment analysis on DE variables | No | GP |
| **Advanced Analysis** |  |  |  |
| [`step_infer_structure()`](https://glycoverse.github.io/glysmith/dev/reference/step_infer_structure.md) | Infer glycan structures from glycan compositions | No | GP/G |
| [`step_derive_traits()`](https://glycoverse.github.io/glysmith/dev/reference/step_derive_traits.md) | Calculate glycan derived traits | **Yes** | GP/G |
| [`step_quantify_dynamic_motifs()`](https://glycoverse.github.io/glysmith/dev/reference/step_quantify_dynamic_motifs.md) | Quantify dynamic glycan motifs | **Yes** | GP/G |
| [`step_quantify_branch_motifs()`](https://glycoverse.github.io/glysmith/dev/reference/step_quantify_branch_motifs.md) | Quantify N-glycan branch motifs | **Yes** | GP/G |
| [`step_roc()`](https://glycoverse.github.io/glysmith/dev/reference/step_roc.md) | ROC analysis for biomarker discovery | No | GP/G |
| **Survival Analysis** |  |  |  |
| [`step_cox()`](https://glycoverse.github.io/glysmith/dev/reference/step_cox.md) | Cox proportional hazards model | No | GP/G |

*GP: glycoproteomics, G: glycomics*

## Choose individual preprocessing operations

Use
[`step_preprocess()`](https://glycoverse.github.io/glysmith/dev/reference/step_preprocess.md)
for glyclean’s full automatic workflow. To control the method or order,
put the individual operations directly in the blueprint. For example,
when zeros encode missing measurements, you can filter by missingness
within each group, impute the retained features, and normalize each
sample to its total abundance.

``` r

bp <- blueprint(
  step_zero_to_na() %=>%
  step_remove("rare", prop = 0.5, by = "group") %=>%
  step_impute("min_prob", seed = 42) %=>%
  step_normalize("total_area") %=>%
  step_plot_qc(when = "post", method = "rle", by = "group")
)
```

Each cleaning step reads the experiment on its incoming connection. The
root input and intermediate experiments are available through
[`cast_data()`](https://glycoverse.github.io/glysmith/dev/reference/cast_exp.md).
Use `method = "auto"` for automatic selection within a single operation,
such as imputation. For an explicit method, supply the arguments of the
matching glyclean function. For example, `step_impute("fw_knn", k = 3)`
calls
[`glyclean::impute_fw_knn()`](https://glycoverse.github.io/glyclean/reference/impute_fw_knn.html)
with three neighbors.
