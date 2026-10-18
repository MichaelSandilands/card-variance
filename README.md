# Card payments variance

A weekly pipeline and Shiny app for validating the Central Bank of Ireland's
[Monthly Card Payment Statistics](https://www.centralbank.ie/statistics/data-and-analysis/monthly-card-payment-statistics)
before regulatory submission. It computes MoM, QoQ (complete quarters only), YoY
and YTD variances. It checks each release for structure, completeness,
reconciliation and plausibility, records what changed since the last release, and
shows it all with downloadable evidence.

```
Central Bank CKAN ──► targets pipeline (GitHub Actions, weekly) ──► Cloudflare R2 ──► Shiny app (Posit Connect)
                      download · hash · snapshot · tidy ·             versioned pins:      reads releases only,
                      variances · validation · revisions              raw files, datasets,  never recalculates
                                                                      releases, run log
```

## Quick start (synthetic data, no credentials)

```sh
# from the project root, R >= 4.3 (renv bootstraps itself the first time R starts here)
Rscript -e 'renv::restore()'     # once renv.lock is committed; until then: renv::install()

export CARDVAR_SOURCE=fixtures CARDVAR_BOARD=local
CARDVAR_FIXTURE_DIR=tests/fixtures/release_2026-07 Rscript run_pipeline.R   # first release
CARDVAR_FIXTURE_DIR=tests/fixtures/release_2026-08 Rscript run_pipeline.R   # next release: revisions appear
Rscript -e 'shiny::runApp("app")'
```

The fixtures are **synthetic**, shaped like the Bank's files, and the app shows a
banner saying so. Four problems are planted so every check has something to find:
a June 2025 reconciliation break, a May 2026 revision, an August 2026
Accommodation spike, and a missing February 2024 month for Leitrim
(`tests/fixtures/make_fixtures.R` documents them).

## Production setup

1. **R2 bucket.** Create a bucket, then two API tokens under *R2 > Manage R2 API tokens*:
   *Object Read & Write* for the pipeline and *Object Read* for the app.
2. **Pipeline secrets.** In the GitHub repository, under *Settings > Secrets and variables > Actions*,
   add the secrets `R2_ACCESS_KEY_ID` and `R2_SECRET_ACCESS_KEY` (read-write token), and
   `R2_ACCOUNT_ID` and `R2_BUCKET` as secrets or, since neither is sensitive, as variables.
   Optionally add the variable `R2_PREFIX` (default `card-variance/`). `.github/workflows/refresh.yml` then runs every Monday at 06:00 UTC,
   and you can also start it by hand from the *Actions* tab. Each run starts clean and compares
   with what is already in R2, so a week with no new data writes only the run log. GitHub turns
   off scheduled workflows in public repositories after 60 days without a commit; if that happens,
   re-enable it on the *Actions* tab.
3. **First live run.** Run the workflow once by hand and check the points under
   *First live run* below.
4. **Deploy the app.** Set the read-only token in `.Renviron` (see `.Renviron.example`),
   then run `Rscript deploy/deploy_connect.R`. The script copies the variables to the
   content's Vars panel on Connect, where they are stored encrypted.

To schedule on Posit Connect instead of GitHub, deploy `refresh_connect.Rmd` (instructions inside)
and give it a weekly schedule.

Set `source.user_agent` in `config/datasets.yml` to a real contact address. The Bank's site
refuses some automated clients, so confirm the download works from wherever the job runs.

## Package versions with renv

The project uses renv (`.Rprofile`, `renv/activate.R`, `renv/settings.json`) and `renv.lock` is committed.
`renv::restore()` installs exactly the locked versions.

- **CI** (`tests.yml`) installs exactly the locked versions and R version.
- **Posit Connect.** `deploy/deploy_connect.R` runs inside the renv project, so the app is bundled with the
  locked versions. The optional `refresh_connect.Rmd` deploy carries `renv.lock` itself.
- **Changing packages.** Edit `DESCRIPTION`, run `renv::install()` and `renv::snapshot()`, and commit both.
  The tests workflow fails if the lockfile and `DESCRIPTION` disagree.

The snapshot is `explicit`: it records what `DESCRIPTION` lists (and their dependencies), not whatever
the code happens to load.

## First live run

The CSVs could not be opened while this was built, so these points are confirmed
on the first real run. The app's *About* tab shows every column with the role it was given.

- [ ] **Columns.** `config/schemas/mpci.csv`, `mpcp.csv` and `mpcy.csv` come from the
      data.gov.ie data dictionaries, which date from 2024. If the files have changed, the
      schema check stops that dataset and lists the differences. If you accept them, update the CSV.
- [ ] **Merchant category file.** Its dictionary is not published, so it has no approved column list.
      The first run publishes it with a *Review* flag and writes
      `config/schemas/mpmc.observed.csv` (the column list also appears in the *About* tab).
      Check it, then save it as `mpmc.csv`.
- [ ] **Filter labels.** Columns without an entry under `labels:` show their code in sentence case.
      Add labels for any new codes.
- [ ] **Reconciliation.** The pipeline learns which totals equal the sum of their parts from past
      months (see below). Review the learned rules on the *Validation* tab. Add explicit rules
      or cross-file checks under `rules:` and `cross_checks:` where you know the identity.
- [ ] **Counties.** The boundary file (Tailte Éireann, CC BY 4.0) is downloaded to
      `data/ref/counties.geojson` (on every GitHub run, since each starts clean). The *Every county in the data has a boundary* check confirms
      the names match.
- [ ] **Population (optional).** For the map's *per 1,000 residents* view, add
      `config/county_population.csv` with columns `county,population`, using the CSO Census 2022
      figures (PxStat table FP001 or the Census 2022 Main Results).

## How a weekly run works

1. Download the four CSVs. targets hashes each one, so nothing downstream runs if a
   file has not changed. The Bank publishes monthly, so most weekly runs stop here.
2. Store every distinct raw file as a pin version in R2, so any release can be reproduced.
3. Profile columns. A code column `X` with a label `X_DESC` becomes one filter showing the label.
   Constant columns move to the *About* tab, and columns that repeat another one-to-one (such as
   `UNITS` beside `UNIT_MEASURE_DESC`) are hidden.
4. Tidy each file into one long table, compute all four variances, validate, and compare with the previous raw release.
5. Publish each dataset as a versioned pin, then a release pin that names the dataset
   versions it contains. A new release is written only when content changed. A dataset with a
   *Stop* failure is not published over its previous release.
6. Append to the run log, so the app shows *last checked* separately from *data last changed*.

`run_pipeline.R` exits with status 2 when a dataset was blocked, so the scheduler alerts you.

## Variances

| Variance | Flow (payments) | Stock (cards issued) | Valid only when |
| --- | --- | --- | --- |
| MoM | Month vs previous month | Same | Both months published |
| QoQ | Sum of 3 months vs previous quarter | Quarter-end month | Both quarters have all 3 months |
| YoY | Month vs same month a year earlier | Same | Both months published |
| YTD | Jan–month total vs same months a year earlier | Jan–month average vs same months a year earlier | Every month since January published in both years |

Comparisons join on dates, so a gap yields "no comparison" rather than a wrong pair.
Percent change is `(current − comparison) / |comparison|`; a zero base gives no percentage.
With data from March 2022, Q1 2022 is excluded and the first valid YTD comparison is 2024 vs 2023.

## Validation checks

| Category | Check | Severity |
| --- | --- | --- |
| Structure | Columns match the approved list; periods parse; values are numbers; one row per series and month | Stop |
| Structure | Frequency is monthly | Fail |
| Completeness | No missing months inside a series | Fail (Review for series below the materiality share) |
| Completeness | Blank observations; series that stop early | Review |
| Reconciliation | A total equals the sum of its parts (learned: held in ≥ 95% of earlier months; months with a blank part are skipped) | Fail |
| Reconciliation | No part exceeds its total (where parts overlap, such as contactless inside point of sale) | Review |
| Reconciliation | Rules and cross-file checks you add in `config/datasets.yml` | As configured |
| Plausibility | No negative values | Fail |
| Plausibility | Latest MoM beyond 20% or YoY beyond 30% (material series only) | Review |
| Plausibility | Latest month outside STL/IQR fences (material series only; the same fit the *Diagnostics* tab draws) | Review |
| Plausibility | Average transaction value moved more than 15% | Review |
| Revisions | Published values removed since the previous release | Fail |
| Revisions | Earlier values revised; more than one new month | Review |
| Cross-file | All files end in the same month; county names match boundaries | Review / Fail |

Tolerance for sums is half the smallest published unit for each value involved. Thresholds are in
`config/datasets.yml`, globally or for one file under `datasets.<id>.thresholds`: the merchant category
file, with 363 categories, reviews only series above 1% of the largest in their unit. A known event can be recorded in `config/annotations.csv`
(`dataset,month,series_pattern,note`). It then marks matching movement or anomaly flags as explained and appears
on the trend chart.

## The app

- **Release header.** On every tab: the dataset and month, when it was published and checked, and
  the check result in words ("2 checks failed and 2 need review before sign-off"), counting the
  cross-file checks that cover the dataset.
- **Sidebar.** Grouped into Dataset, Filters and Comparison: file, release (any earlier release can be
  reopened), measure, filters that cascade, variance type and period.
- **About.** Source and licence, lineage (SHA-256, raw version, retrieval time), caveats,
  definitions, and every column with the reason it is shown or hidden.
- **Trend.** A headline line (latest value, MoM, QoQ, YoY, YTD, each with its comparison period), then
  level lines with the chosen variance beneath on one x-axis. Excluded partial quarters are listed.
  With YTD chosen, a chart of each year's January-to-month figure shows this year against earlier years.
  Charts and tables expand to full screen.
- **Variance table.** Both source values beside every change, filterable to rows needing attention.
- **Diagnostics.** timetk time series, seasonality, STL decomposition and anomalies.
- **Map** (county file). Choropleth of value, per 1,000 residents, share, or % change, with a sorted table.
- **Validation** and **Revisions.** Each check expands to the rows behind it.
- **Downloads** on every tab: CSV, or Excel with a metadata sheet recording the source, hash, release, filters and definition.

## Layout

```
_targets.R            pipeline definition
run_pipeline.R        scheduled entry point (writes pipeline_summary.md)
R/                    pipeline functions: ingest, profile, tidy, variance, validate, revisions, geo, bundle
app/                  Shiny app (deploy this folder); app/R/shared.R and storage.R are also used by the pipeline
config/datasets.yml   datasets, labels, thresholds, optional rules
config/schemas/       approved column lists
config/annotations.csv known events
tests/testthat/       unit tests (112)
tests/fixtures/       synthetic releases and their generator
deploy/               Posit Connect deployment
renv/, renv.lock      renv setup and locked package versions
.github/workflows/    weekly refresh and tests
.Renviron.example     the R2 variables the app and deploy script read
```

## Tests

```sh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

The tests cover the variance rules (gaps, partial quarters, stock vs flow, YTD completeness, zero
and negative bases), profiling, schema checks, learned reconciliation, gaps and duplicates,
anomalies with annotations, revisions, county names and the app helpers. CI also runs the pipeline twice on the fixtures.

## Data notes

- Households only; business cards are excluded, and credit and debit payments are not split.
- County is the terminal's location. Online spend is booked to the merchant's head office, which inflates Dublin.
- The series breaks from the discontinued Credit and Debit Card Statistics (Table A.13).
- Data © Central Bank of Ireland; county boundaries © Tailte Éireann (CC BY 4.0). Local testing uses
  Natural Earth boundaries (public domain) merged to 26 counties.
