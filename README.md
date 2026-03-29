# ETL Commodity Prices Pipeline

A production-structured R ETL pipeline that extracts international fertilizer
commodity prices from the World Bank and monthly PHP/USD exchange rates from
the Bangko Sentral ng Pilipinas (BSP), transforms and joins them, and loads
net-new records into a SQLite database (PostgreSQL-ready via DBI).

Built to demonstrate real data engineering competence: automated structure
discovery, modular architecture, idempotent loading, and full test coverage.

---

## What It Does

```
World Bank CMO Monthly XLSX  ──┐
                                ├─► Ingest ─► Transform ─► Integrate ─► Delta Check ─► SQLite / PostgreSQL
BSP Peso/Dollar XLSX         ──┘
```

**Commodities tracked:** phosphate rock, DAP, TSP, urea, potassium chloride
**Granularity:** monthly
**Output:** USD prices + PHP equivalents per commodity per month
**Idempotency:** re-running never creates duplicate records

---

## Stack

| Layer | Technology |
|---|---|
| Language | R |
| HTTP / download | `httr2`, base `download.file` |
| DOM scraping | `rvest` |
| Excel parsing | `readxl` |
| Data manipulation | `dplyr`, `tidyr`, `lubridate` |
| Database | `DBI` + `RSQLite` (PostgreSQL-ready) |
| Testing | `testthat` |
| Config | `dotenv`, `here` |
| Reproducible environment | `renv` |

---

## Project Structure

```
etl-commodity-prices/
├── run_pipeline.R              # Master controller — entry point
├── Makefile                    # make run | test | clean | setup
├── .env.example                # All config keys — copy to .env
├── renv.lock                   # Exact package version snapshot
├── LICENSE
│
├── R/
│   ├── config.R                # PIPELINE_CONFIG — all values from .env
│   ├── utils/
│   │   ├── log.R               # Structured logger: [ LABEL ] message
│   │   ├── network.R           # Pre-flight check, polite delay, retry wrapper
│   │   └── fs.R                # Directory creation, file validation
│   ├── extract/
│   │   ├── extract_wb_commodity.R   # World Bank XLSX — automated link discovery
│   │   └── extract_bsp.R            # BSP XLSX — direct download, stable URL
│   ├── ingest/
│   │   ├── ingest_wb.R              # Auto-detect sheet, header, data rows
│   │   └── ingest_bsp.R             # Auto-detect sheet, header, ghost columns
│   ├── transform/
│   │   ├── transform_wb.R           # Clean, type-cast, sentinel handling
│   │   └── transform_bsp.R          # Value-based column detection, fill merged cells
│   ├── integrate/
│   │   └── integrate.R              # Join + USD→PHP conversion
│   └── load/
│       ├── delta_check.R            # Anti-join — net-new rows only
│       └── load_sqlite.R            # DBI write — SQLite default, PostgreSQL-ready
│
└── tests/
    ├── test_transform_wb.R     # Transform correctness + sentinel handling
    └── test_integrate.R        # Join logic, NA propagation, sort order
```

---

## Setup

**Prerequisites:** R ≥ 4.2, `make`

```bash
# 1. Clone the repository
git clone https://github.com/yourusername/etl-commodity-prices.git
cd etl-commodity-prices

# 2. Restore the exact package environment
make setup
# or directly:
Rscript -e "renv::restore()"

# 3. Configure environment
cp .env.example .env
# Open .env — defaults work out of the box for SQLite
```

`.env` defaults are ready to run with no changes. All values are documented
in `.env.example`.

---

## Usage

```bash
# Run the full pipeline
make run

# Run the test suite
make test

# Remove all staged files and the database
make clean
```

### Adding a commodity

Edit `WB_COLUMNS` in `.env`:

```
WB_COLUMNS=phosphate_rock,dap,tsp,urea,potassium_chloride,natural_gas
```

Zero script changes required. The pipeline resolves commodity columns by
regex at runtime — unit suffixes in the World Bank file are handled
automatically.

### Switching to PostgreSQL

```
LOAD_TARGET=postgres
PG_CONNECTION_STRING=postgresql://user:password@host:5432/dbname
```

Install `RPostgres`: `install.packages("RPostgres")`. No other changes.

---

## Data Sources

| Source | Data | Credentials |
|---|---|---|
| [World Bank Commodity Markets](https://www.worldbank.org/en/research/commodity-markets) | Monthly fertilizer prices (USD/MT) | None |
| [Bangko Sentral ng Pilipinas (BSP)](https://www.bsp.gov.ph/statistics/external/pesodollar.xlsx) | Monthly PHP/USD exchange rates | None |

Both sources are fully public. No API keys required.

---

## Design Decisions

**Automated link discovery (World Bank):** No hardcoded filename or version
number. Two-pass regex over all XLSX hrefs on the page survives World Bank
file renaming or page restructuring without any code or config changes.

**Auto-detect sheet and header (Excel ingestion):** No hardcoded sheet names,
skip values, or row positions. Both ingestors scan the raw matrix to locate
data dynamically — robust to BSP and World Bank file restructuring.

**Value-based column detection (BSP):** BSP has no reliable year or month
header. Column roles are detected by inspecting data values — year column
identified by 4-digit integer pattern, month column by month name pattern,
rate column by header keyword then numeric fallback. Never positional.

**Merged cell handling (BSP):** BSP year column uses Excel merged cells —
only the first month of each year carries the year value. `tidyr::fill()`
propagates the year downward before any filtering occurs.

**Type alignment before join:** World Bank produces month as integer (1–12).
BSP produces month as character ("January"...). BSP month names are converted
to integer via `match(month, month.name)` before the join — consistent types,
no silent mismatches.

**Idempotent load:** Delta check via anti-join on composite key (year, month)
before any write. Re-running the pipeline on an up-to-date database exits
cleanly with zero writes.

**DBI abstraction:** SQLite and PostgreSQL share identical load code. The
connection is the only thing that changes — controlled entirely by `.env`.

**BSP as exchange rate source:** BSP is the primary source — the authoritative
institution that sets and publishes PHP/USD rates for the Philippines. The
World Bank PA.NUS.FCRF indicator is itself derived from BSP data. Going to
the primary source is the stronger engineering decision. Monthly granularity
matches the World Bank commodity data exactly.

---

## Output Schema

Table: `commodity_prices`

| Column | Type | Description |
|---|---|---|
| `year` | INTEGER | Calendar year |
| `month` | INTEGER | Month number (1–12) |
| `phosphate_rock` | REAL | Price USD/MT |
| `dap` | REAL | Price USD/MT |
| `tsp` | REAL | Price USD/MT |
| `urea` | REAL | Price USD/MT |
| `potassium_chloride` | REAL | Price USD/MT |
| `phosphate_rock_php` | REAL | PHP equivalent |
| `dap_php` | REAL | PHP equivalent |
| `tsp_php` | REAL | PHP equivalent |
| `urea_php` | REAL | PHP equivalent |
| `potassium_chloride_php` | REAL | PHP equivalent |
| `exchange_rate_avg` | REAL | Monthly avg PHP/USD (BSP) |

---

## Author

**Lance Angelo Arcega** — CS practitioner
Philippines · [GitHub](https://github.com/yourusername)