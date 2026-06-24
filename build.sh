#!/usr/bin/env bash
rm -f gofigR*tar.gz
git rm -f man/*Rd
rm -f man/*Rd

set -e
R -e "remotes::install_deps(dependencies=TRUE)"
R -e "roxygen2::roxygenize(clean=TRUE)"
git add man/*Rd
export _R_CHECK_DEPENDS_ONLY_=true
# Skip the CRAN-submission-only "incoming feasibility" check, which fetches
# CRAN's package DB over the network. Our repo points at PPM's Linux binary
# endpoint (no src/contrib/Meta/current.rds), so it 404s and fails the check.
export _R_CHECK_CRAN_INCOMING_REMOTE_=false
R CMD build .
R CMD check --as-cran gofigR*tar.gz


echo "Checking Rd..."
for f in $(ls man/*Rd); do if ! grep -Pq "\\\\value" "$f"; then echo $f; fi; done
