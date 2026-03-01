# Get Started with glysmith

Glycoverse is an incredibly comprehensive ecosystem. On one hand, this
means you can flexibly perform a wide range of analyses by combining its
different components. On the other hand, getting started can require
learning quite a lot.

`glysmith` is the highest-level package in the glycoverse suite. It
encapsulates the most common analysis workflows into a unified
interface, allowing you to execute the entire analytical pipeline in
just one function call.

If you’ve seen The Lord of the Rings, you can think of `glysmith` as the
“One Ring to rule them all.”

*Note: `glysmith` is not part of the core glycoverse packages. You need
to install it separately, even if you have installed the meta package
`glycoverse`.*

``` r
library(glysmith)
library(glyexp)
```

## Analyze your data with a single line of code

We will demonstrate using the built-in experiment object from `glyexp`.
This is an N-glycoproteomics dataset containing 12 samples across four
different liver conditions: healthy controls (H), hepatitis (M),
cirrhosis (Y), and hepatocellular carcinoma (C), with 3 samples for each
group. Please note, the dataset is raw and has not been normalized or
imputed.

To work with your own data, you can use `glyread` to import outputs from
tools like pGlyco3, MSFragger-Glyco, or Byonic; Alternatively, you can
create your own experiment object. For more details, see [Get Started
with
glyread](https://glycoverse.github.io/glyread/articles/glyread.html) or
[Creating
Experiments](https://glycoverse.github.io/glyexp/articles/create-exp.html).

``` r
real_experiment
#> 
#> ── Glycoproteomics Experiment ──────────────────────────────────────────────────
#> ℹ Expression matrix: 12 samples, 4262 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <glyrpr_c>, glycan_structure <glyrpr_s>
```

Now, let’s run the default analysis pipeline with just a single line of
code.

``` r
result <- forge_analysis(real_experiment)
result
```

That’s it! You’ve completed the following steps in one go:

- Preprocessed your data using
  [`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html)
  and generated QC plots.
- Summarized the experiment with
  [`glyexp::summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/summarize_experiment.html).
- Performed principal component analysis with
  [`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html).
- Conducted differential expression analysis using
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).
- Plotted a volcano plot via
  [`glyvis::plot_volcano()`](https://glycoverse.github.io/glyvis/reference/plot_volcano.html).
- Generated a heatmap for significant glycoforms using
  [`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).
- Conducted GO enrichment analysis using
  [`glystats::gly_enrich_go()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).
- Performed KEGG enrichment analysis using
  [`glystats::gly_enrich_kegg()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).
- Performed Reactome enrichment analysis using
  [`glystats::gly_enrich_reactome()`](https://glycoverse.github.io/glystats/reference/gly_enrich_go.html).
- Derived site-specific traits using
  [`glydet::derive_traits()`](https://glycoverse.github.io/glydet/reference/derive_traits.html).
- Conducted differential trait analysis with
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html).
- Generated a heatmap of significant site-specific derived traits via
  [`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).

What a comprehensive analysis pipeline!

## Save all results with one line of code

Once you have your results, what’s next? You can export all tables and
plots to a target directory in just a single line of code.

``` r
quench_result(result, "path/to/save")
```

Running this will create the folder `path/to/save` with the following
content:

- `README.md`: Describes all the saved outputs.
- `experiment.rds`: The processed experiment object.
- `meta.rds`: The metadata list.
- `plots/`: All plots generated from the pipeline.
- `tables/`: All tables output from the analysis.

Give it a try yourself!

## One line to generate a report

We’re developing advanced report generation features, but for now, you
can use
[`polish_report()`](https://glycoverse.github.io/glysmith/reference/polish_report.md)
to generate a report directly from the results.

``` r
polish_report(result, "path/to/save/report.html")
```

By default, the report will open automatically in your browser when it’s
finished.

## About naming

You may have noticed some unusual names in this package. First, the
‘smith’ in `glysmith` doesn’t refer to a surname; it’s taken from
“blacksmith”—someone who makes and repairs items made of iron.

All functions and classes are named using blacksmith jargon. For
example,
[`forge_analysis()`](https://glycoverse.github.io/glysmith/reference/forge_analysis.md)
for “forging” results,
[`quench_result()`](https://glycoverse.github.io/glysmith/reference/quench_result.md)
for “quenching” the results to save them, and
[`polish_report()`](https://glycoverse.github.io/glysmith/reference/polish_report.md)
for “polishing” the report to improve readability.

You’ll learn about “blueprint” in the next section.

## Blueprints

Imagine you visit a blacksmith to order a sword. If you simply say, “I
want a sword,” the blacksmith will use a default blueprint and start
working on it. However, you always have the option to bring your own
design—a custom blueprint—that the smith can use for your sword.

Similarly,
[`forge_analysis()`](https://glycoverse.github.io/glysmith/reference/forge_analysis.md)
uses a default blueprint for the analysis pipeline. You can inspect it
via
[`blueprint_default()`](https://glycoverse.github.io/glysmith/reference/blueprint_default.md).

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

A blueprint is a list of steps executed sequentially. You can use
[`blueprint()`](https://glycoverse.github.io/glysmith/reference/blueprint.md)
with `step_` functions to create your own custom blueprint. For
instance, if you’d just like to perform differential expression analysis
and plot the volcano plot, you could do:

``` r
bp <- blueprint(
  step_preprocess(),
  step_dea_limma(),
  step_volcano(),
)
result <- forge_analysis(real_experiment, bp)
result
```

You can also save a blueprint to a file and reload it later:

``` r
write_blueprint(bp, "path/to/save/bp.rds")
bp <- read_blueprint("path/to/save/bp.rds")
```

To find out more about creating blueprints, check out [this
vignette](https://glycoverse.github.io/glysmith/articles/blueprint.html).
You can also have AI to create a blueprint for you. Check out [this
vignette](https://glycoverse.github.io/glysmith/articles/ai.html).

**Happy forging!**
