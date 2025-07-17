#!/bin/env bash
set -e
R CMD INSTALL .
Rscript -e "devtools::test(stop_on_failure=TRUE, reporter = 'teamcity')"
