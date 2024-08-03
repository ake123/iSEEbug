# iSEEbug

[![issues](https://img.shields.io/github/issues/RiboRings/iSEEbug)](https://github.com/RiboRings/iSEEbug/issues)
[![pulls](https://img.shields.io/github/issues-pr/microbiome/iSEEbug)](https://github.com/microbiome/iSEEbug/pulls)
[![R-CMD-check](https://github.com/RiboRings/iSEEbug/workflows/rworkflows/badge.svg)](https://github.com/RiboRings/iSEEbug/actions)
[![codecov](https://codecov.io/gh/RiboRings/iSEEbug/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/RiboRings/iSEEbug?branch=devel)
[![codefactor](https://www.codefactor.io/repository/github/RiboRings/iseebug/badge)](https://www.codefactor.io/repository/github/RiboRings/iseebug)

The goal of iSEEbug is to provide a user-friendly interface to import, build,
explore and visualise TreeSummarizedExperiment objects by means of
[_iSEE_](https://isee.github.io/).

## Installation instructions
The devel version of iSEEbug can be installed from GitHub as follows:

```
remotes::install_github("RiboRings/iSEEbug")
```

## Example
The basic functionality of iSEEbug can be explored as follows:

```
library(iSEEbug)

app <- iSEEbug()

# Launch iSEE
if (interactive()) {
  shiny::runApp(app)
}
```

## Code of Conduct
Please note that the iSEEbug project is released with a
[Contributor Code of Conduct](https://bioconductor.org/about/code-of-conduct/).
By contributing to this project, you agree to abide by its terms. Contributions
are welcome in the form of feedback, issues and pull requests. You can find the
contributor guidelines of the miaverse
[here](https://github.com/microbiome/mia/blob/devel/CONTRIBUTING.md).

## Acknowledgements
Please note that iSEEbug was only made possible thanks to many other R and
bioinformatics software authors, which are cited in the vignettes describing
this package.

This package was developed using the following resources:

- [_usethis_](https://cran.r-project.org/web/packages/usethis/) to generate an
  initial template.
- Continuous code testing is performed on
  [GitHub actions](https://github.com/features/actions) and include R CMD check,
  [_BiocCheck_](https://bioconductor.org/packages/3.16/bioc/html/BiocCheck.html)
  and testthat.
- Code coverage assessment is possible thanks to
  [codecov](https://app.codecov.io/gh/).
- The documentation website is automatically updated thanks to
  [_pkgdown_](https://cran.r-project.org/web/packages/pkgdown/).
- The documentation is formatted thanks to
  [_devtools_](https://cran.r-project.org/web/packages/devtools/) and
  [_roxygen2_](https://cran.r-project.org/web/packages/roxygen2/).
- All the actions above are made reproducible by
  [_rworkflows_](https://neurogenomics.github.io/rworkflows/)