#' Filter the experiment used for a differential fit
#'
#' In a connected blueprint, attach this step to a differential analysis.
#' Multiple filters can consume one fit with different cutoffs. A successful
#' filter can return an experiment with zero rows.
#'
#' @inheritParams glystats::filter_sig_vars
#' @param ... Method-specific arguments for [glystats::filter_sig_vars()],
#'   such as `on = "post_hoc_test"` and `comparison` for ANOVA.
#' @return A connected step with a filtered experiment as its main output.
#' @examples
#' blueprint(step_dea_anova() %=>% step_filter_sig_vars())
#' @export
step_filter_sig_vars <- function(
  p_adj_cutoff = 0.05,
  p_val_cutoff = NULL,
  fc_cutoff = NULL,
  ...
) {
  args <- rlang::list2(
    p_adj_cutoff = p_adj_cutoff,
    p_val_cutoff = p_val_cutoff,
    fc_cutoff = fc_cutoff,
    ...
  )
  supplied <- names(as.list(match.call(expand.dots = TRUE))[-1L])
  args <- args[intersect(names(args), supplied)]
  step(
    id = "filter_sig_vars",
    label = "Filter significant variables",
    packages = "glystats",
    connected = .fit_port("filtered_exp", "selected"),
    run = function(ctx) {
      options <- utils::modifyList(ctx$filter_args, args, keep.null = TRUE)
      selected <- rlang::exec(
        glystats::filter_sig_vars,
        ctx$data$exp,
        ctx$data$dea_res,
        !!!options
      )
      ctx_add_data(ctx, "selected", selected)
    },
    report = function(x) {
      sprintf(
        "Retained %d of %d fitted variables.",
        nrow(x$data$selected),
        nrow(x$exp)
      )
    },
    signature = rlang::expr_deparse(match.call())
  )
}
