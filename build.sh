#!/usr/bin/env bash
set -e
rm -f gofigR*tar.gz
R -e "roxygen2::roxygenize(clean=TRUE)"
R CMD build .
R CMD check --as-cran gofigR*tar.gz
