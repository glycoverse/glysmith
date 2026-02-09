# Tests for step_cox (survival analysis)

test_that("step_cox creates a valid step", {
  step_obj <- step_cox()
  expect_s3_class(step_obj, "glysmith_step")
  expect_equal(step_obj$id, "cox")
  expect_equal(step_obj$label, "Cox proportional hazards model")
})

test_that("step_cox with on parameter creates proper id and label", {
  step_obj <- step_cox(on = "sig_exp")
  expect_equal(step_obj$id, "cox_sig")
  # Label format uses "of" instead of parentheses
  expect_true(grepl("significant variables", step_obj$label))
  expect_true(grepl("Cox", step_obj$label))
})

test_that("step_cox with custom time and event columns works", {
  step_obj <- step_cox(time_col = "survival_time", event_col = "death")
  expect_s3_class(step_obj, "glysmith_step")
})

test_that("step_cox with no p-adjustment works", {
  step_obj <- step_cox(p_adj_method = NULL)
  expect_s3_class(step_obj, "glysmith_step")
})

test_that("step_cox condition passes when time_col exists", {
  # Create mock experiment with survival data
  exp <- glyexp::real_experiment2
  exp$sample_info$time <- runif(nrow(exp$sample_info), 1, 100)
  exp$sample_info$event <- sample(0:1, nrow(exp$sample_info), replace = TRUE)

  mock_ctx <- structure(
    list(
      data = list(exp = exp)
    ),
    class = "glysmith_context"
  )

  step_obj <- step_cox()
  condition_result <- step_obj$condition(mock_ctx)

  expect_equal(condition_result$check, TRUE)
  expect_null(condition_result$reason)
})

test_that("step_cox condition fails when time_col missing", {
  exp <- glyexp::real_experiment2
  # Add event but not time
  exp$sample_info$event <- sample(0:1, nrow(exp$sample_info), replace = TRUE)

  mock_ctx <- structure(
    list(
      data = list(exp = exp)
    ),
    class = "glysmith_context"
  )

  step_obj <- step_cox(time_col = "missing_time")
  condition_result <- step_obj$condition(mock_ctx)

  expect_equal(condition_result$check, FALSE)
  expect_true(grepl("not found in sample info", condition_result$reason))
})

test_that("step_cox condition fails when event_col missing", {
  exp <- glyexp::real_experiment2
  # Add time but not event
  exp$sample_info$time <- runif(nrow(exp$sample_info), 1, 100)

  mock_ctx <- structure(
    list(
      data = list(exp = exp)
    ),
    class = "glysmith_context"
  )

  step_obj <- step_cox(event_col = "missing_event")
  condition_result <- step_obj$condition(mock_ctx)

  expect_equal(condition_result$check, FALSE)
  expect_true(grepl("not found in sample info", condition_result$reason))
})

test_that("step_cox condition fails when experiment missing", {
  mock_ctx <- structure(
    list(
      data = list()
    ),
    class = "glysmith_context"
  )

  step_obj <- step_cox()
  condition_result <- step_obj$condition(mock_ctx)

  expect_equal(condition_result$check, FALSE)
  expect_true(grepl("not found", condition_result$reason))
})

test_that("step_cox report function works with results", {
  skip_on_ci()
  skip_on_cran()

  # Create mock experiment with survival data
  exp <- glyexp::real_experiment2
  n_samples <- nrow(exp$sample_info)
  exp$sample_info$time <- runif(n_samples, 1, 100)
  exp$sample_info$event <- sample(0:1, n_samples, replace = TRUE)

  suppressMessages(
    exp <- exp |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )

  bp <- blueprint(step_cox())
  suppressMessages(res <- forge_analysis(exp, bp))

  step <- res$blueprint$cox
  suppressMessages(report_output <- step$report(res))
  expect_type(report_output, "character")
  expect_true(nchar(report_output) > 0)
  expect_true(grepl("Cox", report_output))
  expect_true(grepl("Number of variables", report_output))
})

test_that("step_cox report handles empty results", {
  # Create minimal result with empty cox table
  bp <- blueprint(step_cox())
  res <- structure(
    list(
      tables = list(
        cox = data.frame(
          variable = character(),
          coefficient = numeric(),
          std.error = numeric(),
          statistic = numeric(),
          p_val = numeric(),
          hr = numeric(),
          p_adj = numeric()
        )
      ),
      blueprint = bp
    ),
    class = "glysmith_result"
  )

  step <- res$blueprint$cox
  report_output <- step$report(res)
  expect_equal(report_output, "No Cox regression results available.")
})

test_that("step_cox generates table with expected columns", {
  skip_on_ci()
  skip_on_cran()

  # Create mock experiment with survival data
  exp <- glyexp::real_experiment2
  n_samples <- nrow(exp$sample_info)
  exp$sample_info$time <- runif(n_samples, 1, 100)
  exp$sample_info$event <- sample(0:1, n_samples, replace = TRUE)

  suppressMessages(
    exp <- exp |>
      glyexp::slice_head_var(5) |>
      glyclean::auto_clean()
  )

  bp <- blueprint(step_cox())
  suppressMessages(res <- forge_analysis(exp, bp))

  cox_table <- res$tables$cox
  expect_true(nrow(cox_table) > 0)
  expect_true("variable" %in% colnames(cox_table))
  expect_true("coefficient" %in% colnames(cox_table))
  expect_true("hr" %in% colnames(cox_table))
  expect_true("p_adj" %in% colnames(cox_table))
})

test_that("step_cox stores raw result", {
  skip_on_ci()
  skip_on_cran()

  # Create mock experiment with survival data
  exp <- glyexp::real_experiment2
  n_samples <- nrow(exp$sample_info)
  exp$sample_info$time <- runif(n_samples, 1, 100)
  exp$sample_info$event <- sample(0:1, n_samples, replace = TRUE)

  suppressMessages(
    exp <- exp |>
      glyexp::slice_head_var(5) |>
      glyclean::auto_clean()
  )

  bp <- blueprint(step_cox())
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("cox_raw_res" %in% names(res$data))
})

test_that("step_cox with bonferroni adjustment works", {
  step_obj <- step_cox(p_adj_method = "bonferroni")
  expect_s3_class(step_obj, "glysmith_step")
})
