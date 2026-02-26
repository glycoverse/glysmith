.on_choices <- function() {
  c(
    "exp",
    "sig_exp",
    "trait_exp",
    "sig_trait_exp",
    "dynamic_motif_exp",
    "sig_dynamic_motif_exp",
    "branch_motif_exp",
    "sig_branch_motif_exp"
  )
}

.has_glycan_structure <- function(exp) {
  "glycan_structure" %in% colnames(exp$var_info)
}

#' Resolve properties from 'on' parameter
#'
#' @param on Name of the experiment data.
#' @returns A list with suffixes and prefixes.
#' @noRd
.resolve_on <- function(on) {
  checkmate::assert_choice(on, .on_choices())
  list(
    id_suffix = switch(
      on,
      exp = "",
      sig_exp = "_sig",
      trait_exp = "_trait",
      sig_trait_exp = "_sig_trait",
      dynamic_motif_exp = "_dynamic_motif",
      sig_dynamic_motif_exp = "_sig_dynamic_motif",
      branch_motif_exp = "_branch_motif",
      sig_branch_motif_exp = "_sig_branch_motif"
    ),
    label_suffix = switch(
      on,
      exp = "",
      sig_exp = " of significant variables",
      trait_exp = " of traits",
      sig_trait_exp = " of significant traits",
      dynamic_motif_exp = " of dynamic motifs",
      sig_dynamic_motif_exp = " of significant dynamic motifs",
      branch_motif_exp = " of branch motifs",
      sig_branch_motif_exp = " of significant branch motifs"
    )
  )
}
