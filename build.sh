#!/usr/bin/env bash
set -e
R -e "roxygen2::roxygenize(clean=TRUE)"
R CMD build .
R CMD check --as-cran gofigR*tar.gz
