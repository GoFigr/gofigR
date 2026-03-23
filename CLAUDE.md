# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is gofigR

R client package for GoFigr.io, a zero-effort reproducibility engine. Automatically publishes figures from RMarkdown, interactive R sessions, and scripts to the GoFigr service with full metadata (code, data, watermarks).

## Build & Check Commands

```bash
# Full build: regenerates docs, builds tarball, runs R CMD check --as-cran
bash build.sh

# Run tests (installs package first)
bash run_tests.sh

# Manual equivalents:
R -e "roxygen2::roxygenize(clean=TRUE)"
R CMD build .
R CMD check --as-cran gofigR*.tar.gz
R CMD INSTALL .
Rscript -e "devtools::test(stop_on_failure=TRUE)"
```

Tests are integration tests that require GoFigr credentials (env vars `GF_USERNAME`, `GF_PASSWORD` or `GF_API_KEY`) and `torch`/`torchvision` for image similarity comparison. All tests skip on CRAN via `skip_on_cran()`.

## Architecture

### Object Model

All API objects (clients, workspaces, analyses, figures, revisions, assets) are represented as **R environments** (not S3 classes). This gives pass-by-reference semantics and avoids copying large nested structures. The `response_to_JSON()` function recursively converts JSON lists into environments.

### Layer Structure

- **`api.R`** — HTTP client, JWT/API-key auth, token refresh, request helpers. All other modules go through this.
- **`workspace.R`, `analysis.R`, `figure.R`, `revision.R`, `asset.R`** — CRUD operations for each entity. All follow a consistent pattern: `list_*`, `get_*`, `create_*`, `find_*`, `delete_*`.
- **`data.R`** — Constructors for revision data objects: `make_image_data()`, `make_code_data()`, `make_text_data()`, `make_file_data()`, `make_table_data()`.
- **`integrations.R`** (largest file, ~820 lines) — The main orchestration layer: `publish()`, `enable()`, plot interception via `intercept()`, file sync wrappers (`read.csv`, `read_csv`, etc.), and the `with_isolated_devices()` graphics device manager.
- **`watermark.R`** — QR code watermark generation with dynamic sizing. Three modes: `QR_WATERMARK`, `LINK_WATERMARK`, `NO_WATERMARK`.
- **`context.R`** — Detects execution environment (knitr, RStudio, script, interactive) and captures appropriate metadata.
- **`gfconfig.R`** — Interactive configuration wizard, writes `~/.gofigr`.
- **`shiny_components.R`** — Shiny UI/server integration for interactive figure publishing.

### Key Patterns

- **Find-or-create**: `find_or_create()` looks up entities by name, creates if `create=TRUE`, errors on duplicates.
- **Plot interception**: `intercept()` wraps `plot`/`print` to auto-publish. Controlled by `is_intercept_on()` and `suppress()`.
- **Content-addressable assets**: Files synced via BLAKE3 hash to prevent duplicate uploads.
- **Isolated graphics devices**: `with_isolated_devices()` snapshots open devices before/after to clean up only new ones.
- **Credential resolution order**: function args → env vars (`GF_USERNAME`, `GF_PASSWORD`, `GF_API_KEY`, `GF_WORKSPACE`, `GF_URL`) → config file (`~/.gofigr`).

### API

Base URL: `https://api.gofigr.io/api/v1.2/`

## Documentation

Uses roxygen2. All `.Rd` files in `man/` are auto-generated — edit roxygen comments in `R/*.R` files, then run `roxygen2::roxygenize()`. The build script verifies every `.Rd` file has a `\value` section (CRAN requirement).

## CI

GitHub Actions (`.github/workflows/ci.yml`): runs on push/PR to main/develop using `rocker/verse:4.3` container. Runs `build.sh` then `run_tests.sh`.
