# glysmith

> **One Ring to rule them all.** — *The Lord of the Rings*

![](reference/figures/ring.png)

Be overwhelmed by the complexity of glycoverse?

Try glysmith! Perform the comprehensive analysis pipeline with one
function call.

## Installation

You can install the development version of glysmith from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("glycoverse/glysmith")
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
