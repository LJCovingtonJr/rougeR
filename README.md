# rougeR
An implementation of the ROUGE package for the automatic evaluation of summaries.  My first actual package.  See the citation below for more information.

Currently supports the generation of ROUGE-N and -S.  Support for ROUGE-L and -W, are in development.

## Installation
This package is not available on CRAN, because reasons.  To get the package from GitHub, use [devtools](https://github.com/hadley/devtools).
``` r
# install.packages("devtools")
devtools::install_github("LJCovingtonJr/rougeR")
```

## Reference
Lin, C., 2004. ROUGE: A Package for Automatic Evaluation of Summaries. [online] Barcelona, Spain: Association for Computational Linguistics, pp.74-81. Available at: <https://www.aclweb.org/anthology/W04-1013/> [Accessed 6 April 2020].
