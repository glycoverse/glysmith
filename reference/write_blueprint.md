# Save or Load a Blueprint

- `write_blueprint()` saves a blueprint to a RDS file.

- `read_blueprint()` loads a blueprint from a RDS file.

## Usage

``` r
write_blueprint(bp, file)

read_blueprint(file)
```

## Arguments

- bp:

  A
  [`blueprint()`](https://glycoverse.github.io/glysmith/reference/blueprint.md).

- file:

  A character string giving the name of the file to save to or load
  from.

## Value

Invisibly returns the blueprint object.

## Examples

``` r
bp <- blueprint(
  step_preprocess(),
  step_pca(),
  step_dea_limma(),
)
write_blueprint(bp, tempfile(fileext = ".rds"))
```
