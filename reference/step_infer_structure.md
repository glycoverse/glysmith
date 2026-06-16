# Step: Infer Glycan Structures

Infer glycan structures from the `glycan_composition` column in
`var_info`. This step uses
[`glyanno::comp_to_struc()`](https://glycoverse.github.io/glyanno/reference/comp_to_struc.html)
with a structure database from
[`glydb::glydb_structures()`](https://glycoverse.github.io/glydb/reference/glydb_structures.html)
and keeps only variables with an inferred structure.

This step requires `exp` (experiment data).

## Usage

``` r
step_infer_structure(species = NULL, structure_level = "topological")
```

## Arguments

- species:

  Species name used to restrict the glycan structure database. Default
  is `NULL`, which does not restrict by species.

- structure_level:

  Structure level passed to
  [`glydb::glydb_structures()`](https://glycoverse.github.io/glydb/reference/glydb_structures.html).
  One of `"intact"`, `"topological"`, or `"basic"`. Default is
  `"topological"`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment whose glycan structures should be inferred

Data generated:

- `uninferred_exp`: The original experiment before structure inference

Tables generated:

- `inferred_structures`: A table containing the inferred structure for
  each original variable and whether inference succeeded.

## See also

[`glyanno::comp_to_struc()`](https://glycoverse.github.io/glyanno/reference/comp_to_struc.html),
[`glydb::glydb_structures()`](https://glycoverse.github.io/glydb/reference/glydb_structures.html)

## Examples

``` r
step_infer_structure()
#> <step "step_infer_structure()"> Glycan structure inference
step_infer_structure(species = "Homo sapiens")
#> <step "step_infer_structure(species = \"Homo sapiens\")"> Glycan structure
#> inference
```
