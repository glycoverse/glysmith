.on_choices <- function() {
  c(
    "exp",
    "sig_exp",
    "trait_exp",
    "sig_trait_exp",
    "motif_exp",
    "sig_motif_exp"
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
      motif_exp = "_motif",
      sig_motif_exp = "_sig_motif"
    ),
    label_suffix = switch(
      on,
      exp = "",
      sig_exp = " of significant variables",
      trait_exp = " of traits",
      sig_trait_exp = " of significant traits",
      motif_exp = " of motifs",
      sig_motif_exp = " of significant motifs"
    )
  )
}
