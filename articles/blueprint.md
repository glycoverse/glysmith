# Custom Blueprint

``` r
library(glysmith)
```

A blueprint is simply an analytical plan that tells
[`forge_analysis()`](https://glycoverse.github.io/glysmith/reference/forge_analysis.md)
what to do. By default,
[`forge_analysis()`](https://glycoverse.github.io/glysmith/reference/forge_analysis.md)
runs a default blueprint:

``` r
blueprint_default()
#> 
#> ── Blueprint (13 steps) ──
#> 
#> • step_ident_overview()
#> • step_preprocess()
#> • step_plot_qc(when = "post")
#> • step_pca()
#> • step_dea_limma()
#> • step_volcano()
#> • step_heatmap(on = "sig_exp")
#> • step_sig_enrich_go()
#> • step_sig_enrich_kegg()
#> • step_sig_enrich_reactome()
#> • step_derive_traits()
#> • step_dea_limma(on = "trait_exp")
#> • step_heatmap(on = "sig_trait_exp")
```

This vignette shows you how to build your own blueprint for custom
analyses.

**Heads up:** Before diving into the details of manual blueprint
creation, you might want to try having an LLM generate one for you with
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md).
Check out [this
vignette](https://glycoverse.github.io/glysmith/articles/ai.html) for
the details.

## Step functions

`glysmith` comes with various step functions (anything starting with
`step_`), each handling a specific analytical task. Chain them together
with
[`blueprint()`](https://glycoverse.github.io/glysmith/reference/blueprint.md):

``` r
bp <- blueprint(
  step_ident_overview(),  # Identification overview
  step_preprocess(),      # Preprocess (normalization, imputation, etc.)
  step_dea_limma(),       # DEA with limma
  step_volcano(),         # Volcano plot
)
```

Now `bp` is ready to go — just pass it as the second argument to
[`forge_analysis()`](https://glycoverse.github.io/glysmith/reference/forge_analysis.md):

``` r
res <- forge_analysis(exp, bp)
```

Here’s the full lineup of available step functions:

| Function                                                                                                            | Description                                                 | Require Glycan Structure | Experiment Type |
|:--------------------------------------------------------------------------------------------------------------------|:------------------------------------------------------------|:------------------------:|:---------------:|
| **Preprocessing**                                                                                                   |                                                             |                          |                 |
| [`step_ident_overview()`](https://glycoverse.github.io/glysmith/reference/step_ident_overview.md)                   | Summarize the experiment (identification overview)          |            No            |      GP/G       |
| [`step_plot_qc()`](https://glycoverse.github.io/glysmith/reference/step_plot_qc.md)                                 | Generate quality control plots                              |            No            |      GP/G       |
| [`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md)                           | Preprocess the experiment (normalization, imputation, etc.) |            No            |      GP/G       |
| [`step_subset_groups()`](https://glycoverse.github.io/glysmith/reference/step_subset_groups.md)                     | Subset the experiment to specific groups                    |            No            |      GP/G       |
| [`step_adjust_protein()`](https://glycoverse.github.io/glysmith/reference/step_adjust_protein.md)                   | Adjust glycoform quantification by protein abundance        |            No            |       GP        |
| **Differential Analysis**                                                                                           |                                                             |                          |                 |
| [`step_dea_limma()`](https://glycoverse.github.io/glysmith/reference/step_dea_limma.md)                             | Differential expression analysis using limma                |            No            |      GP/G       |
| [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/reference/step_dea_ttest.md)                             | Differential expression analysis using t-test               |            No            |      GP/G       |
| [`step_dea_anova()`](https://glycoverse.github.io/glysmith/reference/step_dea_anova.md)                             | Differential expression analysis using ANOVA                |            No            |      GP/G       |
| [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/reference/step_dea_wilcox.md)                           | Differential expression analysis using Wilcoxon test        |            No            |      GP/G       |
| [`step_dea_kruskal()`](https://glycoverse.github.io/glysmith/reference/step_dea_kruskal.md)                         | Differential expression analysis using Kruskal-Wallis test  |            No            |      GP/G       |
| **Visualization**                                                                                                   |                                                             |                          |                 |
| [`step_volcano()`](https://glycoverse.github.io/glysmith/reference/step_volcano.md)                                 | Create volcano plot from DEA results                        |            No            |      GP/G       |
| [`step_heatmap()`](https://glycoverse.github.io/glysmith/reference/step_heatmap.md)                                 | Create heatmap plot                                         |            No            |      GP/G       |
| [`step_logo()`](https://glycoverse.github.io/glysmith/reference/step_logo.md)                                       | Create logo plot for glycosylation sites                    |            No            |       GP        |
| [`step_sig_boxplot()`](https://glycoverse.github.io/glysmith/reference/step_sig_boxplot.md)                         | Boxplots for significant variables from DEA                 |            No            |      GP/G       |
| **Dimension Reduction**                                                                                             |                                                             |                          |                 |
| [`step_pca()`](https://glycoverse.github.io/glysmith/reference/step_pca.md)                                         | Principal Component Analysis                                |            No            |      GP/G       |
| [`step_tsne()`](https://glycoverse.github.io/glysmith/reference/step_tsne.md)                                       | t-SNE analysis                                              |            No            |      GP/G       |
| [`step_umap()`](https://glycoverse.github.io/glysmith/reference/step_umap.md)                                       | UMAP analysis                                               |            No            |      GP/G       |
| [`step_plsda()`](https://glycoverse.github.io/glysmith/reference/step_plsda.md)                                     | Partial Least Squares Discriminant Analysis                 |            No            |      GP/G       |
| [`step_oplsda()`](https://glycoverse.github.io/glysmith/reference/step_oplsda.md)                                   | Orthogonal Partial Least Squares Discriminant Analysis      |            No            |      GP/G       |
| **Correlation & Enrichment**                                                                                        |                                                             |                          |                 |
| [`step_correlation()`](https://glycoverse.github.io/glysmith/reference/step_correlation.md)                         | Pairwise correlation analysis                               |            No            |      GP/G       |
| [`step_sig_enrich_go()`](https://glycoverse.github.io/glysmith/reference/step_sig_enrich_go.md)                     | GO enrichment analysis on DE variables                      |            No            |       GP        |
| [`step_sig_enrich_kegg()`](https://glycoverse.github.io/glysmith/reference/step_sig_enrich_kegg.md)                 | KEGG enrichment analysis on DE variables                    |            No            |       GP        |
| [`step_sig_enrich_reactome()`](https://glycoverse.github.io/glysmith/reference/step_sig_enrich_reactome.md)         | Reactome enrichment analysis on DE variables                |            No            |       GP        |
| **Advanced Analysis**                                                                                               |                                                             |                          |                 |
| [`step_derive_traits()`](https://glycoverse.github.io/glysmith/reference/step_derive_traits.md)                     | Calculate glycan derived traits                             |         **Yes**          |      GP/G       |
| [`step_quantify_dynamic_motifs()`](https://glycoverse.github.io/glysmith/reference/step_quantify_dynamic_motifs.md) | Quantify dynamic glycan motifs                              |         **Yes**          |      GP/G       |
| [`step_quantify_branch_motifs()`](https://glycoverse.github.io/glysmith/reference/step_quantify_branch_motifs.md)   | Quantify N-glycan branch motifs                             |         **Yes**          |      GP/G       |
| [`step_roc()`](https://glycoverse.github.io/glysmith/reference/step_roc.md)                                         | ROC analysis for biomarker discovery                        |            No            |      GP/G       |
| **Survival Analysis**                                                                                               |                                                             |                          |                 |
| [`step_cox()`](https://glycoverse.github.io/glysmith/reference/step_cox.md)                                         | Cox proportional hazards model                              |            No            |      GP/G       |

*GP: glycoproteomics, G: glycomics*

## Step dependencies

Some steps need data that other steps produce. For instance,
[`step_volcano()`](https://glycoverse.github.io/glysmith/reference/step_volcano.md)
needs results from a DEA step like
[`step_dea_limma()`](https://glycoverse.github.io/glysmith/reference/step_dea_limma.md).
So this blueprint won’t work:

``` r
blueprint(  # this will error
  step_preprocess(),      # Preprocess (normalization, imputation, etc.)
  step_volcano(),         # Volcano plot
)
```

The easiest way to figure out dependencies is to check the documentation
for each step. For example, `?step_volcano()` tells you it “requires one
of the DEA steps to be run:
[`step_dea_limma()`](https://glycoverse.github.io/glysmith/reference/step_dea_limma.md),
[`step_dea_ttest()`](https://glycoverse.github.io/glysmith/reference/step_dea_ttest.md),
[`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/reference/step_dea_wilcox.md)”.
So as long as you run one of these DEA steps before
[`step_volcano()`](https://glycoverse.github.io/glysmith/reference/step_volcano.md),
you’re good to go:

``` r
bp <- blueprint(
  step_preprocess(),      # Preprocess (normalization, imputation, etc.)
  step_dea_ttest(),       # DEA with t-test
  step_volcano(),         # Volcano plot
)
```

Steps and their dependencies don’t need to be right next to each other —
all intermediate data gets stored along the way, so the blueprint is
valid as long as the required data exists somewhere upstream:

``` r
bp <- blueprint(
  step_preprocess(),      # Preprocess (normalization, imputation, etc.)
  step_dea_ttest(),       # DEA with t-test (generates data required by `step_volcano()`)
  step_heatmap(),         # Heatmap
  step_volcano(),         # Volcano plot (requires data generated by `step_dea_ttest()`)
)
```

## The `on` parameter

Many step functions have an `on` parameter that controls their
dependencies. For example,
[`step_heatmap()`](https://glycoverse.github.io/glysmith/reference/step_heatmap.md)
defaults to `on = "exp"`, which plots a heatmap from the experiment
(after preprocessing, if you did that). But you can point it at other
data like “sig_exp” instead — just remember you’ll need to run a DEA
step first:

``` r
bp <- blueprint(
  step_preprocess(),             # Preprocess (normalization, imputation, etc.)
  step_dea_limma(),              # DEA with limma
  step_heatmap(on = "sig_exp"),  # Heatmap on significantly expressed variables
)
```

Check `?step_dea_limma()` and you’ll see it generates a field called
exactly “sig_exp”. This is how data flows in `glysmith`: each step
stores its results in a shared context object, where downstream steps
can grab what they need.

Let’s see another example:

``` r
bp <- blueprint(
  # Data required: `exp`
  # Data generated: `exp` (overwrite)
  step_preprocess(),

  # Data required: `exp`
  # Data generated: `trait_exp` (trait experiment)
  step_derive_traits(),

  # Data required: `trait_exp` (specified by `on`)
  # Data generated: `sig_trait_exp` (significantly different trait experiment)
  step_dea_limma(on = "trait_exp"),

  # Data required: `sig_trait_exp`
  step_heatmap(on = "sig_trait_exp"),
)
```

Here’s the general rule for dataflow in `glysmith`: steps don’t
overwrite data generated by earlier steps. The exception is
[`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md),
which updates `exp` with the preprocessed version (but don’t worry — the
raw data is safely backed up in `raw_exp`).

To build valid blueprints, you’ll need to know what each step needs and
what it produces. Let’s look at a more complex example to cement this.
Try tracing through the dataflow yourself:

``` r
bp <- blueprint(
  step_ident_overview(),
  step_preprocess(),
  step_pca(),
  
  # DEA
  step_dea_limma(),
  step_volcano(),
  step_heatmap(on = "sig_exp"),

  # Derived trait analysis
  step_derive_traits(),
  step_dea_limma(on = "trait_exp"),
  step_heatmap(on = "sig_trait_exp"),

  # Branch motif analysis
  step_quantify_branch_motifs(),
  step_dea_limma(on = "branch_motif_exp"),
  step_heatmap(on = "sig_branch_motif_exp")
)
```

## Branches

As we mentioned, steps can’t overwrite data from earlier steps. But what
if you want to compare different DEA methods? You might try something
like this:

``` r
blueprint(
  step_preprocess(),
  step_dea_limma(),
  step_volcano(),
  step_dea_ttest(),
  step_volcano(),
)
```

This won’t fly for two reasons:

1.  [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/reference/step_dea_ttest.md)
    would overwrite the `sig_exp` from
    [`step_dea_limma()`](https://glycoverse.github.io/glysmith/reference/step_dea_limma.md).
2.  [`step_volcano()`](https://glycoverse.github.io/glysmith/reference/step_volcano.md)
    shows up twice, which isn’t allowed.

These rules keep table and plot names unambiguous.

To pull this off, use **branches**. So far our blueprints have been
linear — steps run one after another. To branch out, just wrap related
steps in
[`br()`](https://glycoverse.github.io/glysmith/reference/br.md):

``` r
bp <- blueprint(
  step_preprocess(),
  br("limma", step_dea_limma(), step_volcano()),
  br("t-test", step_dea_ttest(), step_volcano()),
)
```

The first argument names the branch, and the rest are the steps to run
inside it. Each branch gets its own isolated context, so the
[`step_volcano()`](https://glycoverse.github.io/glysmith/reference/step_volcano.md)
in the “limma” branch naturally picks up the `sig_exp` from the
[`step_dea_limma()`](https://glycoverse.github.io/glysmith/reference/step_dea_limma.md)
in that same branch. Branches can also access data from before the
branch point — so both branches here can use the preprocessed `exp` from
[`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md).

## Summary

In this vignette, we covered how to build custom blueprints, how the
`on` parameter and step dependencies work, and how to use branches.

All of this takes some practice, so if you’d rather not dive deep into
the details, remember you can always [have AI generate
blueprints](https://glycoverse.github.io/glysmith/articles/ai.html) for
you.
