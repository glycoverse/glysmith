library(patchwork)
load_all()

exp <- glyexp::real_experiment

bp1 <- blueprint(
  step_preprocess(),
  step_dea(),
  step_sig_enrich_go(universe = "all")
)

bp2 <- blueprint(
  step_preprocess(),
  step_dea(),
  step_sig_enrich_go(universe = "detected")
)

res1 <- forge_analysis(exp, blueprint = bp1)
res2 <- forge_analysis(exp, blueprint = bp2)

cast_plot(res1, "go") + cast_plot(res2, "go")
