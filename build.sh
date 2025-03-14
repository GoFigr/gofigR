#!/usr/bin/env bash
set -e
rm -f gofigR*tar.gz
rm -f man/*Rd
R -e "roxygen2::roxygenize(clean=TRUE)"
R CMD build .
R CMD check --as-cran gofigR*tar.gz


echo "Checking Rd..."
for f in $(ls man/*Rd); do if ! grep -Pq "\\\\value" "$f"; then echo $f; fi; done
