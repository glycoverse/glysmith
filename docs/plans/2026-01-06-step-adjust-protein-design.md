# Step Adjust Protein Design

## Overview
Add a new blueprint step, `step_adjust_protein()`, that removes protein abundance from glycoform quantifications by calling `glyclean::adjust_protein()`. The step reads a protein expression matrix from a file path (CSV, TSV, or RDS), adjusts the experiment, overwrites `exp`, and stores the original as `raw_exp`.

## Goals
- Provide a simple step API to perform protein adjustment within a blueprint.
- Support protein expression matrices from file paths (CSV, TSV, RDS).
- Surface `glyclean` adjustment messages in the report output.
- Match existing step patterns for context mutation and reporting.

## Non-goals
- Supporting in-memory matrices (future extension).
- Re-implementing `glyclean` adjustment logic.
- Building a separate data-loading step for protein matrices.

## API
```
step_adjust_protein(pro_expr_path, method = c("ratio", "reg"))
```

- `pro_expr_path`: Path to a protein expression matrix file.
- `method`: Adjustment method passed to `glyclean::adjust_protein()`.

## File Format Contract
- CSV/TSV: The first column contains protein accessions and becomes row names.
  Remaining columns are sample names and must match `exp` sample names.
- RDS: A matrix or data.frame with row names as protein accessions and
  columns as sample names.

## Data Flow
1. Read protein matrix from `pro_expr_path`.
2. Fetch `exp` from context.
3. Call `glyclean::adjust_protein(exp, pro_expr_mat, method)`.
4. Overwrite `exp` with adjusted experiment and store original as `raw_exp`.

## Error Handling
- Abort on unsupported file extensions.
- Abort when CSV/TSV has fewer than two columns, missing accessions, or
  non-unique accessions.
- Abort when RDS lacks row names or has non-unique accessions.
- Rely on `glyclean` to error when experiment type, protein columns, or
  shared proteins/samples are invalid.

## Reporting
- Report function extracts captured `message` logs from the step execution and
  appends them to the step summary.
- If no messages exist, fall back to a minimal summary sentence.

## Testing
- Add tests in `tests/testthat/test-step.R`.
- Create a small protein matrix using `real_experiment` proteins and sample
  names, write to CSV/TSV/RDS, and validate:
  - `raw_exp` is created.
  - `exp` is adjusted and differs from `raw_exp`.
- Ensure the report includes adjustment messages when available.

## Open Questions
- None.
