# Makefile — ETL Commodity Prices Pipeline
# ─────────────────────────────────────────────────────────────────────────────
# Usage:
#   make run        — execute the full pipeline end-to-end
#   make test       — run the testthat suite
#   make clean      — remove all staged and output files
#   make setup      — install all required R packages
#
# Prerequisites: R must be installed and on PATH. Copy .env.example → .env.
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: run test clean setup

run:
	Rscript run_pipeline.R

test:
	Rscript -e "testthat::test_file('tests/test_transform_wb.R')"
	Rscript -e "testthat::test_file('tests/test_integrate.R')"

clean:
	rm -rf raw_data/* outputs/* etl.sqlite

setup:
	Rscript -e "renv::restore()"
