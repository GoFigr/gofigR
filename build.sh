#!/usr/bin/env bash
rm -f gofigR*tar.gz
git rm -f man/*Rd
rm -f man/*Rd

set -e
R -e "remotes::install_deps(dependencies=TRUE)"
R -e "roxygen2::roxygenize(clean=TRUE)"
git add man/*Rd
R CMD build .
R CMD check --as-cran gofigR*tar.gz


echo "Checking Rd..."
for f in $(ls man/*Rd); do if ! grep -Pq "\\\\value" "$f"; then echo $f; fi; done
