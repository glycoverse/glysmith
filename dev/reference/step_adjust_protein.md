# Step: Adjust Protein Abundance

Adjust glycoform quantification values by correcting for protein
abundance utilizing
[`glyclean::adjust_protein()`](https://glycoverse.github.io/glyclean/reference/adjust_protein.html).
Usually this step should be run after
[`step_preprocess()`](https://glycoverse.github.io/glysmith/dev/reference/step_preprocess.md).

This step requires `exp` (experiment data).

## Usage

``` r
step_adjust_protein(pro_expr_path = NULL, method = "ratio")
```

## Arguments

- pro_expr_path:

  Path to the protein expression matrix file. If `NULL`, this step will
  be skipped. Can be:

  - A CSV/TSV file with the first column as protein accessions and
    remaining columns as sample names.

  - An RDS file with a matrix or data.frame with row names as protein
    accessions and columns as sample names.

- method:

  The method to use for protein adjustment. Either "ratio" or "reg".
  Default is "ratio".

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to adjust

Data generated:

- `unadj_exp`: The original experiment (previous `exp`, saved for
  reference)

This step is special in that it silently overwrites the `exp` data with
the adjusted experiment. This ensures that no matter if adjustment is
performed or not, the "active" experiment is always under the key `exp`.
The previous `exp` is saved as `unadj_exp` for reference.

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Include this step only if the user explicitly asks for protein
  adjustment.

- If protein adjustment is needed and the `pro_expr_path` is not
  provided, ask for it and explain how to prepare the file:

  - CSV/TSV: first column is protein accessions; remaining columns are
    sample names.

  - RDS: a matrix/data.frame with row names as protein accessions and
    columns as sample names.

- You MUST provide a detailed explanation of how to prepare the file.

- With out the file, the step is invalid.

## See also

[`glyclean::adjust_protein()`](https://glycoverse.github.io/glyclean/reference/adjust_protein.html)

## Examples

``` r
fake_pro_expr_mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
rownames(fake_pro_expr_mat) <- paste0("P", seq_len(10))
colnames(fake_pro_expr_mat) <- paste0("S", seq_len(10))
fake_pro_expr_path <- tempfile(fileext = ".rds")
saveRDS(fake_pro_expr_mat, fake_pro_expr_path)
step_adjust_protein(fake_pro_expr_path)
#> <step "step_adjust_protein(pro_expr_path = fake_pro_expr_path)"> Protein
#> adjustment
```
