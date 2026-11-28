# construction checks every link, including independent sub-blueprints

    Code
      print(bp)
    Output
      Blueprint (root accepts exp)
      +- node_1  step_ident_overview() [overview] (terminal)
      \- node_2  step_derive_traits() (exp)
        +- node_3  step_dea_anova() [tests] (omnibus)
        | +- node_4  step_filter_sig_vars(p_adj_cutoff = 0.01) [tests] (filtered_exp)
        | \- node_5  step_filter_sig_vars() [tests] (filtered_exp)
        |   \- node_6  step_heatmap() [tests] (terminal)
        \- node_7  step_pca() [scores] (terminal)

