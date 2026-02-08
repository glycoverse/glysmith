
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glysmith <a href="https://glycoverse.github.io/glysmith/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/glysmith)](https://CRAN.R-project.org/package=glysmith)
[![R-CMD-check](https://github.com/glycoverse/glysmith/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/glycoverse/glysmith/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/glycoverse/glysmith/graph/badge.svg)](https://app.codecov.io/gh/glycoverse/glysmith)
<!-- badges: end -->

> **One Ring to rule them all.** — *The Lord of the Rings*

<img src="man/figures/ring.png" style="width:25.0%" />

Be overwhelmed by the complexity of glycoverse?

Imagine a package, performing all analysis you need, PCA, DEA,
enrichment, or even advanced glycan derived trait analysis, in just one
line of code.

Try glysmith! Perform the comprehensive analysis pipeline with one
function call.

<https://github.com/user-attachments/assets/d1eede5f-f919-428b-a5e8-574a8ab6f863>

## Installation

`glysmith` is a high-level package, depending on quite a lot mature
packages. For best user experimence, please install the following
packages before installing `glysmith`: `pROC`, `Rtsne`, `uwot`,
`EnhancedVolcano`, `org.Hs.eg.db`, `clusterProfiler`, `ReactomePA`,
`ggplotify`, `pheatmap`, `factoextra`, `FSA`, `ggseqlogo`, `missForest`,
`Hmisc`, `GGally`, `survival`, `UniProt.ws`, `ropls`. We recommend using
`pak::pak()` to install these packages.

You can install the latest release of glysmith from
[r-universe](https://glycoverse.r-universe.dev/glysmith):

``` r
install.packages('glysmith', repos = c('https://glycoverse.r-universe.dev', 'https://cloud.r-project.org'))
```

Or from [GitHub](https://github.com/glycoverse/glysmith):

``` r
# install.packages("pak")
pak::pak("glycoverse/glysmith@*release")
```

## Example

``` r
library(glyread)
library(glysmith)

exp <- read_pglyco3("pglyco3_result.txt", sample_info = "sample_info.csv")

# One line of code for the comprehensive analysis pipeline
result <- forge_analysis(exp)

# One line of code to save the results
quench_result(result, "path/to/save")

# One line of code to generate a report
polish_report(result, "report.html")
```

After running the above `quench_result` line, you will get an organized
result folder:

![](man/figures/result-folder.png)

And after running the above `polish_report` line, you will get a report
in HTML format and automatically open it in your default browser:

![](man/figures/report.png)

*Note: Here we set `use_ai` to `TRUE` to enable LLM-assisted report
generation.*
