# Step: Cox Proportional Hazards Model

Perform survival analysis by fitting a Cox proportional hazards model
using
[`glystats::gly_cox()`](https://glycoverse.github.io/glystats/reference/gly_cox.html)
for each variable. This step identifies variables associated with
survival outcomes.

This step depends on the `on` parameter (default: `exp`).

- When `on = "exp"`, requires `exp` (usually after
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/dev/reference/step_preprocess.md)).

- When `on = "sig_exp"`, requires `sig_exp` from one of
  [`step_dea_limma()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_limma.md),
  [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_ttest.md),
  [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_wilcox.md),
  [`step_dea_anova()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_anova.md),
  or
  [`step_dea_kruskal()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_kruskal.md).

- When `on = "trait_exp"`, requires `trait_exp` from
  [`step_derive_traits()`](https://glycoverse.github.io/glysmith/dev/reference/step_derive_traits.md).

- When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on
  traits.

- When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from
  [`step_quantify_dynamic_motifs()`](https://glycoverse.github.io/glysmith/dev/reference/step_quantify_dynamic_motifs.md).

- When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp`
  from DEA on motifs.

- When `on = "branch_motif_exp"`, requires `branch_motif_exp` from
  [`step_quantify_branch_motifs()`](https://glycoverse.github.io/glysmith/dev/reference/step_quantify_branch_motifs.md).

- When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp`
  from DEA on motifs.

## Usage

``` r
step_cox(
  on = "exp",
  time_col = "time",
  event_col = "event",
  p_adj_method = "BH",
  ...
)
```

## Arguments

- on:

  Name of the experiment to run Cox regression on. Can be "exp",
  "sig_exp", "trait_exp", "sig_trait_exp", "dynamic_motif_exp",
  "sig_dynamic_motif_exp", "branch_motif_exp", "sig_branch_motif_exp".

- time_col:

  Column name in sample information containing survival time. Default is
  "time".

- event_col:

  Column name in sample information containing event indicator (1 for
  event, 0 for censoring). Default is "event".

- p_adj_method:

  Method for adjusting p-values. See `p.adjust.methods`. Default is
  "BH". If NULL, no adjustment is performed.

- ...:

  Additional arguments passed to
  [`glystats::gly_cox()`](https://glycoverse.github.io/glystats/reference/gly_cox.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to run Cox regression on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to run Cox
  regression on

- `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif
  experiment to run Cox regression on

- `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif
  experiment to run Cox regression on

The experiment must contain survival data with `time_col` and
`event_col` columns in the sample information.

Tables generated (with suffixes):

- `cox`: A table containing Cox regression results with columns:

  - `variable`: Variable name

  - `coefficient`: Regression coefficient (log hazard ratio)

  - `std.error`: Standard error of the coefficient

  - `statistic`: Wald test statistic

  - `p_val`: Raw p-value from Wald test

  - `hr`: Hazard ratio (exp(coefficient))

  - `p_adj`: Adjusted p-value (if p_adj_method is not NULL)

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Include this step when users want to identify variables associated
  with survival outcomes.

- This step requires survival data (time and event columns) in the
  sample information.

- Always ask for the column names for survival data, unless explicitly
  provided.

## See also

[`glystats::gly_cox()`](https://glycoverse.github.io/glystats/reference/gly_cox.html),
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)

## Examples

``` r
step_cox()
#> <step "step_cox()"> Cox proportional hazards model
step_cox(time_col = "survival_time", event_col = "death")
#> <step "step_cox(time_col = \"survival_time\", event_col = \"death\")"> Cox
#> proportional hazards model
step_cox(on = "sig_exp", p_adj_method = "bonferroni")
#> <step "step_cox(on = \"sig_exp\", p_adj_method = \"bonferroni\")"> Cox
#> proportional hazards model of significant variables
```
