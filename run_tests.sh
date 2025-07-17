#!/bin/env bash
set -e
Rscript -e "devtools::test(stop_on_failure=TRUE, reporter = 'teamcity')"
