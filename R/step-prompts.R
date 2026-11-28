#' AI guidance for blueprint step selection
#'
#' Internal usage prompts keyed by exported step function name. These prompts are
#' included in the system prompt used by [inquire_blueprint()], but are not
#' exposed in user-facing function documentation.
#'
#' @returns A named character vector.
#' @noRd
.step_ai_prompts <- function() {
  c(
    step_zero_to_na = "Use when zero values represent missing measurements, before filtering or imputation.",
    step_remove = "Use the requested feature filters in order. For rare features, pass the missingness threshold as prop or n and the minimum observed count as min_n. Use by and strict for group-wise filtering.",
    step_normalize = "Select the requested normalization method. Supply by for within-group normalization when supported by that method.",
    step_impute = "Select the requested imputation method. Pass its tuning arguments by name, such as k for nearest neighbors or q, tune.sigma and seed for min_prob.",
    step_aggregate = "Use to combine features at the requested glycan, glycoform, or glycopeptide level.",
    step_detect_batch_effect = "Use when the user asks for per-feature batch effect p-values.",
    step_correct_batch_effect = "Use batch_col, group_col and prop_threshold for automatic correction. For combat or limma, pass the batch and group arguments of the correction method.",
    step_transform_log = "Use when the user wants abundances stored on a specific logarithmic scale.",
    step_adjust_protein = "Include this step only if the user explicitly asks for protein adjustment. If protein adjustment is needed and the `pro_expr_path` is not provided, ask for it and explain how to prepare the file: - CSV/TSV: first column is protein accessions; remaining columns are sample names. - RDS: a matrix/data.frame with row names as protein accessions and columns as sample names. You MUST provide a detailed explanation of how to prepare the file. Without the file, the step is invalid. Set `pro_scale` to the representation actually stored in the protein matrix (raw, log, log2, or log10), independently of the experiment scale. The default is raw; if the user says the file is transformed but does not identify the scale, ask rather than guess.",
    step_coda = "Use for compositional or log-ratio analysis. Run abundance QC, protein adjustment, trait derivation and motif quantification before transforming the main experiment. For compositional analysis of quantified motifs, connect the quantified motifs to the transformation and then to the statistical step. Use significance-based filtering for transformed data and omit volcano plots and fold-change-based enrichment for those results.",
    step_correlation = "Include this step to explore relationships between variables or samples. Be careful to use when sample size or variable number is large (> 50). Before using this step for large data, ask the user if they want to proceed.",
    step_cox = "Include this step when users want to identify variables associated with survival outcomes. This step requires survival data (time and event columns) in the sample information. Always ask for the column names for survival data, unless explicitly provided.",
    step_dea_anova = "Use this step to perform DEA by default for multi-group glycomics data.",
    step_dea_kruskal = "Include this step only if the user explicitly asks for Kruskal-Wallis test.",
    step_dea_limma = "Use this step to perform DEA by default for glycoproteomics data, unless the user asks for other methods.",
    step_dea_ttest = "Use this step to perform DEA by default for 2-group glycomics data. If the experiment has more than 2 groups, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before this step.",
    step_dea_wilcox = "Include this step only if the user explicitly asks for Wilcoxon test. If the experiment has more than 2 groups, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before this step.",
    step_derive_traits = "Include this step by default if the experiment has glycan structures. After this step, it should be followed by the DEA and visualization steps.",
    step_filter_sig_vars = "Connect to a differential fit to emit a filtered experiment.",
    step_heatmap = "Include this step if needed. It is recommended to use this step after step_filter_sig_vars().",
    step_ident_overview = "Always include this step by default unless the user explicitly excludes it. Use it as a side branch at the start in the blueprint.",
    step_infer_structure = "Include this step when the user requests structure-aware analysis but the experiment has glycan compositions and no glycan structures. This step should be placed before `step_derive_traits()`, `step_quantify_motifs()`, `step_quantify_dynamic_motifs()`, or `step_quantify_branch_motifs()`. Mention that variables without inferred structures are removed. Always ask for species restriction to improve inference accuracy, but allow users to skip it if they want.",
    step_logo = "Include this step if the user explicitly asks for logo plot. If used, ask user if a FASTA file is provided. Tell the user that if not, protein sequences will be fetched from UniProt automatically.",
    step_oplsda = "Include this step when users explicitly ask for OPLS-DA. This step only works with binary classification (exactly 2 groups). If multiple groups are found, ask if `step_subset_groups()` should be run first.",
    step_pca = "Include this step if needed.",
    step_plot_glycosite_profile = "Use for glycoproteomics when the user asks about glycan diversity per site or the number of glycosylation sites per protein.",
    step_plot_qc = "By default, include this step after preprocessing. Provide when to specify the QC stage. Set method for a single requested QC plot.",
    step_plsda = "Include this step when users explicitly ask for PLS-DA.",
    step_preprocess = "Use this step by default for automatic preprocessing. If the user specifies a custom preprocessing sequence, use the individual steps in the requested order and omit step_preprocess(). Omit preprocessing when the data is already processed or the user excludes it. Ask for the column name for batch information if not provided. Do not ask for quality-control sample names. For biomarker analysis, use the biomarker removal preset.",
    step_quantify_branch_motifs = "Include this step if motif analysis is needed specifically for N-glycans. This step should be followed by DEA and visualization steps.",
    step_quantify_dynamic_motifs = "Include this step if motif analysis is needed for non-N-glycans or when comprehensive motif extraction is desired. This step should be followed by DEA and visualization steps.",
    step_quantify_motifs = "Include this step when the user wants to quantify a specific or custom set of glycan motifs (by motif names or glycan structure strings in `motifs`). If the user asks for custom motif quantification without specifying which motifs to quantify, ask for the motif names or structure strings. This step should be followed by DEA and visualization steps.",
    step_roc = "Include this step if the user explicitly asks for ROC analysis, or if he/she mentions \"biomarker(s)\" in the prompt. If the experiment has more than 2 groups but the user wants a specific two-group comparison, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before this step.",
    step_sig_boxplot = "Connect this step after step_filter_sig_vars() to visualize the selected variables. This step is particularly useful for understanding the expression patterns of the most differentially expressed features across groups.",
    step_sig_enrich_do = "Include this step if user asks for it. Leave `universe` to \"all\" (by default) unless the user explicitly mentions that the background should be the detected variables in `exp`. If the experiment has more than 2 groups but the user wants enrichment for a specific two-group comparison, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before DEA and enrichment steps.",
    step_sig_enrich_go = "Include this step by default if DEA is performed on glycoproteomics data. Leave `universe` to \"all\" (by default) unless the user explicitly mentions that the background should be the detected variables in `exp`. If the experiment has more than 2 groups but the user wants enrichment for a specific two-group comparison, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before DEA and enrichment steps.",
    step_sig_enrich_kegg = "Include this step by default if DEA is performed on glycoproteomics data. Leave `universe` to \"all\" (by default) unless the user explicitly mentions that the background should be the detected variables in `exp`. If the experiment has more than 2 groups but the user wants enrichment for a specific two-group comparison, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before DEA and enrichment steps.",
    step_sig_enrich_ncg = "Include this step if user asks for it. Leave `universe` to \"all\" (by default) unless the user explicitly mentions that the background should be the detected variables in `exp`. If the experiment has more than 2 groups but the user wants enrichment for a specific two-group comparison, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before DEA and enrichment steps.",
    step_sig_enrich_reactome = "Include this step if user asks for it. Leave `universe` to \"all\" (by default) unless the user explicitly mentions that the background should be the detected variables in `exp`. If the experiment has more than 2 groups but the user wants enrichment for a specific two-group comparison, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before DEA and enrichment steps.",
    step_sig_enrich_wp = "Include this step if user asks for it. Leave `universe` to \"all\" (by default) unless the user explicitly mentions that the background should be the detected variables in `exp`. If the experiment has more than 2 groups but the user wants enrichment for a specific two-group comparison, ask which two groups to compare and include `step_subset_groups(groups = c(\"A\", \"B\"))` before DEA and enrichment steps.",
    step_subset_groups = "Use this step when the experiment has more than 2 groups but the user wants a specific two-group comparison. Ask the user which two groups to compare, and place this step before DEA and enrichment steps. Use the order of the user-provided groups to set factor levels.",
    step_tsne = "Include this step only when the user explicitly asks for t-SNE.",
    step_umap = "Include this step only when the user explicitly asks for UMAP.",
    step_volcano = "Include this step by default for limma, t-test or Wilcoxon DEA results with abundance fold changes. CoDA results use log-ratio coordinates."
  )
}
