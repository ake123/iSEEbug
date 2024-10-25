# miaDash

[![issues](https://img.shields.io/github/issues/RiboRings/miaDash)](https://github.com/RiboRings/miaDash/issues)
[![pulls](https://img.shields.io/github/issues-pr/RiboRings/miaDash)](https://github.com/RiboRings/miaDash/pulls)
[![R-CMD-check](https://github.com/RiboRings/miaDash/workflows/rworkflows/badge.svg)](https://github.com/RiboRings/miaDash/actions)
[![codecov](https://codecov.io/gh/RiboRings/miaDash/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/RiboRings/miaDash?branch=devel)
[![codefactor](https://www.codefactor.io/repository/github/RiboRings/miadash/badge)](https://www.codefactor.io/repository/github/RiboRings/miadash)

The goal of miaDash is to provide a user-friendly interface to import,
manipulate, analyse and visualise TreeSummarizedExperiment objects.

## Usage
miaDash is available online at [this address](iseebug-iseebug.2.rahtiapp.fi/).
While suitable for small and medium datasets, the online version may slow down
when larger datasets are analysed (< 1000 features). In this case, the app can
be installed and run locally. Either way, the app also provides functionality to subset and agglomerate the data.

## Installation instructions
The devel version of miaDash can be installed from GitHub as follows:

```
remotes::install_github("RiboRings/miaDash")
```

## Example
The basic functionality of miaDash can be explored as follows:

```
library(miaDash)

app <- miaDash()

# Launch miaDash
if (interactive()) {
  shiny::runApp(app)
}
```

## Code of Conduct
Please note that the miaDash project is released with a
[Contributor Code of Conduct](https://bioconductor.org/about/code-of-conduct/).
By contributing to this project, you agree to abide by its terms. Contributions
are welcome in the form of feedback, issues and pull requests. You can find the
contributor guidelines of the miaverse
[here](https://github.com/microbiome/mia/blob/devel/CONTRIBUTING.md).

## Acknowledgements
miaDash results from the joint effort of the larger R/Bioconductor community. In
particular, this software mainly depends on the following packages:

- [_mia_](https://bioconductor.org/packages/release/bioc/html/mia.html)
- [_iSEEtree_](https://bioconductor.org/packages/devel/bioc/html/iSEEtree.html)
- [_iSEE_](https://isee.github.io/)
- [_shiny_](https://cran.r-project.org/web/packages/shiny/)
