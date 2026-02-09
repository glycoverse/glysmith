# glysmith

> **One Ring to rule them all.** — *The Lord of the Rings*

![](reference/figures/ring.png)

Be overwhelmed by the complexity of glycoverse?

Imagine a package, performing all analysis you need, PCA, DEA,
enrichment, or even advanced glycan derived trait analysis, in just one
line of code.

Try glysmith! Perform the comprehensive analysis pipeline with one
function call.

<https://github.com/user-attachments/assets/d1eede5f-f919-428b-a5e8-574a8ab6f863>

## Installation

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

`glysmith` is a high-level package, depending on quite a lot mature
packages. After installing `glysmith`, you also need to install
additional dependencies to use this package. Run
[`glysmith::check_glysmith_deps()`](https://glycoverse.github.io/glysmith/reference/check_glysmith_deps.md)
and follow the instructions.

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

![](reference/figures/result-folder.png)

And after running the above `polish_report` line, you will get a report
in HTML format and automatically open it in your default browser:

![](reference/figures/report.png)

*Note: Here we set `use_ai` to `TRUE` to enable LLM-assisted report
generation.*
