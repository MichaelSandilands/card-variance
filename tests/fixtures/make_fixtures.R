# Synthetic fixture data shaped like the Central Bank of Ireland files.
#
# NOT REAL STATISTICS. Levels are anchored loosely on the August 2026 release
# (domestic point of sale EUR 4.43bn, domestic online EUR 3.40bn, non-domestic
# EUR 1.89bn); everything else is invented.
#
# Two releases are written: release_2026-07 (data to July 2026) and
# release_2026-08 (data to August 2026). Planted problems in the August release,
# so every validation check has something to find:
#   1. Card payments, June 2025: world point-of-sale value is EUR 5m above
#      domestic + non-domestic                         -> reconciliation fail
#   2. Card payments, May 2026: domestic online value revised up by EUR 12.3m
#      (totals revised consistently)                   -> revisions warn
#   3. Merchant category, August 2026: Accommodation jumps 45%
#                                                      -> large movement / anomaly warn
#   4. County, Leitrim total value missing for February 2024 -> gap fail
#
# Run from the project root: Rscript tests/fixtures/make_fixtures.R

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source(file.path("R", "utils.R"))
source(file.path("app", "R", "shared.R"))
source(file.path("R", "geo.R"))
set.seed(20261018)

months <- seq(as.Date("2022-03-01"), as.Date("2026-08-01"), by = "month")
anchor <- as.Date("2026-08-01")
season <- c(0.90, 0.88, 0.98, 0.99, 1.02, 1.03, 1.06, 1.07, 0.99, 1.01, 1.04, 1.18)
period_end <- function(m) paste0(format(lubridate::ceiling_date(m, "month") - 1), "T00:00:00")

path_level <- function(base, growth = 0.07, sd = 0.012, seasonal = TRUE) {
  t <- as.numeric(months - anchor) / 365.25
  s <- if (seasonal) season[lubridate::month(months)] / season[8] else 1
  base * (1 + growth)^t * s * exp(stats::rnorm(length(months), 0, sd))
}

sdmx_rows <- function(df, dims, codes_to_desc) {
  for (d in dims) df[[paste0(d, "_DESC")]] <- unname(codes_to_desc[[d]][df[[d]]])
  df
}

# Card payments (mpcp) ----------------------------------------------------------
desc <- list(
  COUNT_AREA = c(W0 = "World", W1 = "Rest of the world", W2 = "Domestic"),
  RMT_INTTN = c(`_T` = "Total", NR = "Non-remote", R = "Remote"),
  INTTN_CHNNL = c(`_T` = "Total", CTLS = "Contactless", MW = "Mobile wallet"),
  TYP_TRNSCTN = c(CP0 = "Card payments", CW1 = "Cash withdrawals"),
  UNIT_MEASURE = c(EUR = "Euro", PN = "Pure number")
)
atv <- c(W2.NR = 24, W2.R = 62, W2.CTLS = 17, W2.MW = 16, W1.NR = 45, W1.R = 70, W1.CTLS = 30, W1.MW = 28)
base <- list(
  W2 = list(NR = 4430, R = 3400, CTLS = 3010, MW = 2190),
  W1 = list(NR = 678.57, R = 1210, CTLS = 400, MW = 280)
)
leaf <- list()
for (a in c("W2", "W1")) {
  nr <- path_level(base[[a]]$NR, 0.06)
  r <- path_level(base[[a]]$R, 0.10)
  ctls <- nr * base[[a]]$CTLS / base[[a]]$NR * (1 + 0.04 * as.numeric(months - anchor) / 365.25)
  mw <- ctls * base[[a]]$MW / base[[a]]$CTLS * (1 + 0.08 * as.numeric(months - anchor) / 365.25)
  leaf[[a]] <- list(NR = nr, R = r, CTLS = pmin(ctls, nr * 0.95), MW = pmin(mw, ctls * 0.95))
}
mk <- function(area, rmt, ch, eur) {
  key <- paste(area, if (ch == "_T") rmt else ch, sep = ".")
  a <- if (area == "W0") "W2" else area
  ratio <- atv[[paste(a, if (ch == "_T") (if (rmt == "_T") "NR" else rmt) else ch, sep = ".")]]
  vol <- eur / ratio * exp(stats::rnorm(length(months), 0, 0.004))
  bind_rows(
    tibble(m = months, COUNT_AREA = area, RMT_INTTN = rmt, INTTN_CHNNL = ch, TYP_TRNSCTN = "CP0",
           UNIT_MEASURE = "EUR", v = eur),
    tibble(m = months, COUNT_AREA = area, RMT_INTTN = rmt, INTTN_CHNNL = ch, TYP_TRNSCTN = "CP0",
           UNIT_MEASURE = "PN", v = vol)
  )
}
cp <- list()
for (a in c("W2", "W1")) {
  L <- leaf[[a]]
  cp[[length(cp) + 1]] <- mk(a, "NR", "_T", L$NR)
  cp[[length(cp) + 1]] <- mk(a, "R", "_T", L$R)
  cp[[length(cp) + 1]] <- mk(a, "NR", "CTLS", L$CTLS)
  cp[[length(cp) + 1]] <- mk(a, "NR", "MW", L$MW)
}
cp <- bind_rows(cp)
# Totals are sums of the unrounded parts, like a compiler would produce them.
w0 <- cp |> group_by(m, RMT_INTTN, INTTN_CHNNL, TYP_TRNSCTN, UNIT_MEASURE) |>
  summarise(v = sum(v), .groups = "drop") |> mutate(COUNT_AREA = "W0")
cp <- bind_rows(cp, w0)
rt <- cp |> filter(INTTN_CHNNL == "_T") |>
  group_by(m, COUNT_AREA, INTTN_CHNNL, TYP_TRNSCTN, UNIT_MEASURE) |>
  summarise(v = sum(v), .groups = "drop") |> mutate(RMT_INTTN = "_T")
cp <- bind_rows(cp, rt)
cash_eur <- path_level(1040, 0.01)
cp <- bind_rows(cp,
  tibble(m = months, COUNT_AREA = "W0", RMT_INTTN = "_T", INTTN_CHNNL = "_T", TYP_TRNSCTN = "CW1",
         UNIT_MEASURE = "EUR", v = cash_eur),
  tibble(m = months, COUNT_AREA = "W0", RMT_INTTN = "_T", INTTN_CHNNL = "_T", TYP_TRNSCTN = "CW1",
         UNIT_MEASURE = "PN", v = cash_eur / 152.5))
cp$v <- round(cp$v, 2)

# Planted problem 1: reconciliation break in June 2025.
hit <- cp$m == as.Date("2025-06-01") & cp$COUNT_AREA == "W0" & cp$RMT_INTTN == "NR" &
  cp$INTTN_CHNNL == "_T" & cp$UNIT_MEASURE == "EUR" & cp$TYP_TRNSCTN == "CP0"
cp$v[hit] <- cp$v[hit] + 5

write_mpcp <- function(d) {
  d |>
    mutate(
      REPORTINGPERIOD = period_end(m),
      SERIES_KEY = paste("PAY.M.IE", COUNT_AREA, "_Z", TYP_TRNSCTN, "1", INTTN_CHNNL, RMT_INTTN,
                         "_Z._Z._Z._Z.N", UNIT_MEASURE, sep = "."),
      OBSERVATION = format(v, nsmall = 2, trim = TRUE, scientific = FALSE),
      CONFIDENTIALITY_FLAG = "F",
      UNITS = ifelse(UNIT_MEASURE == "EUR", "Euro, millions", "Number, millions"),
      FREQ = "M", REF_AREA = "IE", TRMNL_LCTN = "_Z", RL_TRNSCTN = "1", PYMNT_SCHM = "_Z",
      CRD_FNCTN = "_Z", SCA = "_Z", FRD_TYP = "_Z", TRANSFORMATION = "N"
    ) |>
    sdmx_rows(c("COUNT_AREA", "RMT_INTTN", "INTTN_CHNNL", "TYP_TRNSCTN", "UNIT_MEASURE"), desc) |>
    mutate(FREQ_DESC = "Monthly", REF_AREA_DESC = "Ireland", TRMNL_LCTN_DESC = "Not applicable",
           RL_TRNSCTN_DESC = "Payer", PYMNT_SCHM_DESC = "Not applicable",
           CRD_FNCTN_DESC = "All card functions", SCA_DESC = "Not applicable",
           FRD_TYP_DESC = "Not applicable", TRANSFORMATION_DESC = "Non transformed data") |>
    select(REPORTINGPERIOD, SERIES_KEY, OBSERVATION, CONFIDENTIALITY_FLAG, UNITS, FREQ, FREQ_DESC,
           REF_AREA, REF_AREA_DESC, COUNT_AREA, COUNT_AREA_DESC, TRMNL_LCTN, TRMNL_LCTN_DESC,
           TYP_TRNSCTN, TYP_TRNSCTN_DESC, RL_TRNSCTN, RL_TRNSCTN_DESC, INTTN_CHNNL, INTTN_CHNNL_DESC,
           RMT_INTTN, RMT_INTTN_DESC, PYMNT_SCHM, PYMNT_SCHM_DESC, CRD_FNCTN, CRD_FNCTN_DESC,
           SCA, SCA_DESC, FRD_TYP, FRD_TYP_DESC, TRANSFORMATION, TRANSFORMATION_DESC,
           UNIT_MEASURE, UNIT_MEASURE_DESC)
}

# County (mpcy) -----------------------------------------------------------------
weights <- c(CARLOW = 1.2, CAVAN = 1.5, CLARE = 2.5, CORK = 0, DONEGAL = 3.1, DUBLIN = 0,
             GALWAY = 5.2, KERRY = 3.1, KILDARE = 4.4, KILKENNY = 2.1, LAOIS = 1.7, LEITRIM = 0.6,
             LIMERICK = 4.0, LONGFORD = 0.9, LOUTH = 2.6, MAYO = 2.6, MEATH = 3.9, MONAGHAN = 1.2,
             OFFALY = 1.6, ROSCOMMON = 1.3, SLIGO = 1.4, TIPPERARY = 3.0, WATERFORD = 2.5,
             WESTMEATH = 1.9, WEXFORD = 3.2, WICKLOW = 2.8)
share_nr <- weights / sum(weights) * 0.38
share_nr[["DUBLIN"]] <- 0.45; share_nr[["CORK"]] <- 0.10
share_nr <- share_nr / sum(share_nr)
share_r <- share_nr * 0.4; share_r[["DUBLIN"]] <- share_r[["DUBLIN"]] + 0.6 * (1 - sum(share_r)) / 0.6 * 0.6
share_r <- share_r / sum(share_r)
dom <- cp |> filter(COUNT_AREA == "W2", INTTN_CHNNL == "_T", RMT_INTTN %in% c("NR", "R"), TYP_TRNSCTN == "CP0")
cy <- bind_rows(lapply(names(weights), function(cn) {
  dom |> mutate(COUNTY = cn,
                v = v * ifelse(RMT_INTTN == "NR", share_nr[[cn]], share_r[[cn]]) *
                  exp(stats::rnorm(n(), 0, 0.02)))
}))
cy_t <- cy |> group_by(m, COUNTY, UNIT_MEASURE) |> summarise(v = sum(v), .groups = "drop") |>
  mutate(RMT_INTTN = "_T")
cy <- bind_rows(cy |> select(m, COUNTY, RMT_INTTN, UNIT_MEASURE, v), cy_t)
cy_all <- cy |> group_by(m, RMT_INTTN, UNIT_MEASURE) |> summarise(v = sum(v), .groups = "drop") |>
  mutate(COUNTY = "_T")
cy <- bind_rows(cy, cy_all) |> mutate(v = round(v, 2))
county_desc <- c(stats::setNames(tools::toTitleCase(tolower(names(weights))), names(weights)), `_T` = "Total")
# Planted problem 4: a missing month.
cy <- cy |> filter(!(COUNTY == "LEITRIM" & RMT_INTTN == "_T" & UNIT_MEASURE == "EUR" &
                       m == as.Date("2024-02-01")))

write_mpcy <- function(d) {
  d |>
    mutate(
      REPORTINGPERIOD = period_end(m),
      SERIES_KEY = paste("PAY.M.IE.W2", COUNTY, "CP0.1._T", RMT_INTTN, "N", UNIT_MEASURE, sep = "."),
      OBSERVATION = format(v, nsmall = 2, trim = TRUE, scientific = FALSE),
      CONFIDENTIALITY_FLAG = "F",
      UNITS = ifelse(UNIT_MEASURE == "EUR", "Euro, millions", "Number, millions"),
      FREQ = "M", FREQ_DESC = "Monthly", REF_AREA = "IE", REF_AREA_DESC = "Ireland",
      COUNTERPART_AREA = "W2", COUNTERPART_AREA_DESC = "Domestic",
      COUNTY_DESC = unname(county_desc[COUNTY]),
      TYP_TRNSCTN = "CP0", TYP_TRNSCTN_DESC = "Card payments",
      RL_TRNSCTN = "1", RL_TRNSCTN_DESC = "Payer",
      INTTN_CHNNL = "_T", INTTN_CHNNL_DESC = "Total",
      RMT_INTTN_DESC = unname(desc$RMT_INTTN[RMT_INTTN]),
      TRANSFORMATION = "N", TRANSFORMATION_DESC = "Non transformed data",
      UNIT_MEASURE_DESC = unname(desc$UNIT_MEASURE[UNIT_MEASURE])
    ) |>
    select(REPORTINGPERIOD, SERIES_KEY, OBSERVATION, CONFIDENTIALITY_FLAG, UNITS, FREQ, FREQ_DESC,
           REF_AREA, REF_AREA_DESC, COUNTERPART_AREA, COUNTERPART_AREA_DESC, COUNTY, COUNTY_DESC,
           TYP_TRNSCTN, TYP_TRNSCTN_DESC, RL_TRNSCTN, RL_TRNSCTN_DESC, INTTN_CHNNL, INTTN_CHNNL_DESC,
           RMT_INTTN, RMT_INTTN_DESC, TRANSFORMATION, TRANSFORMATION_DESC, UNIT_MEASURE,
           UNIT_MEASURE_DESC)
}

# Merchant category (mpmc) - column layout invented: the real dictionary is not published.
cats <- tribble(
  ~SECTOR, ~SECTOR_DESC, ~MRCHNT_CTGRY, ~MRCHNT_CTGRY_DESC, ~share, ~growth,
  "RET", "Retail", "GRO", "Groceries", 0.23, 0.05,
  "RET", "Retail", "CLO", "Clothing", 0.07, 0.04,
  "RET", "Retail", "AUT", "Automotive", 0.08, 0.03,
  "SOC", "Social", "RES", "Restaurants", 0.09, 0.08,
  "SOC", "Social", "ACC", "Accommodation", 0.05, 0.07,
  "SOC", "Social", "ENT", "Entertainment", 0.04, 0.09,
  "SER", "Services", "FIN", "Financial services", 0.10, -0.10,
  "SER", "Services", "TRA", "Transport", 0.06, 0.06,
  "SER", "Services", "UTL", "Utilities", 0.11, 0.04,
  "MIS", "Miscellaneous", "OTH", "Other", 0.17, 0.05
)
mc <- bind_rows(lapply(seq_len(nrow(cats)), function(i) {
  bind_rows(lapply(c("NR", "R"), function(rmt) {
    b <- if (rmt == "NR") 4430 else 3400
    eur <- path_level(b * cats$share[i], cats$growth[i], 0.02)
    if (cats$MRCHNT_CTGRY[i] == "ACC") eur[length(eur)] <- eur[length(eur) - 1] * 1.45   # planted 3
    bind_rows(
      tibble(m = months, SECTOR = cats$SECTOR[i], MRCHNT_CTGRY = cats$MRCHNT_CTGRY[i],
             RMT_INTTN = rmt, UNIT_MEASURE = "EUR", v = eur),
      tibble(m = months, SECTOR = cats$SECTOR[i], MRCHNT_CTGRY = cats$MRCHNT_CTGRY[i],
             RMT_INTTN = rmt, UNIT_MEASURE = "PN", v = eur / (if (rmt == "NR") 24 else 62))
    )
  }))
}))
mc_rt <- mc |> group_by(m, SECTOR, MRCHNT_CTGRY, UNIT_MEASURE) |> summarise(v = sum(v), .groups = "drop") |>
  mutate(RMT_INTTN = "_T")
mc <- bind_rows(mc, mc_rt)
mc_sec <- mc |> group_by(m, SECTOR, RMT_INTTN, UNIT_MEASURE) |> summarise(v = sum(v), .groups = "drop") |>
  mutate(MRCHNT_CTGRY = "_T")
mc_all <- mc_sec |> group_by(m, RMT_INTTN, UNIT_MEASURE) |> summarise(v = sum(v), .groups = "drop") |>
  mutate(SECTOR = "_T", MRCHNT_CTGRY = "_T")
mc <- bind_rows(mc, mc_sec, mc_all) |> mutate(v = round(v, 2))
sector_desc <- c(stats::setNames(cats$SECTOR_DESC, cats$SECTOR), `_T` = "Total")
cat_desc <- c(stats::setNames(cats$MRCHNT_CTGRY_DESC, cats$MRCHNT_CTGRY), `_T` = "Total")

write_mpmc <- function(d) {
  d |>
    mutate(
      REPORTINGPERIOD = period_end(m),
      SERIES_KEY = paste("PAY.M.IE", SECTOR, MRCHNT_CTGRY, RMT_INTTN, UNIT_MEASURE, sep = "."),
      OBSERVATION = format(v, nsmall = 2, trim = TRUE, scientific = FALSE),
      CONFIDENTIALITY_FLAG = "F",
      UNITS = ifelse(UNIT_MEASURE == "EUR", "Euro, millions", "Number, millions"),
      FREQ = "M", FREQ_DESC = "Monthly", REF_AREA = "IE", REF_AREA_DESC = "Ireland",
      SECTOR_DESC = unname(sector_desc[SECTOR]), MRCHNT_CTGRY_DESC = unname(cat_desc[MRCHNT_CTGRY]),
      RMT_INTTN_DESC = unname(desc$RMT_INTTN[RMT_INTTN]),
      UNIT_MEASURE_DESC = unname(desc$UNIT_MEASURE[UNIT_MEASURE])
    ) |>
    select(REPORTINGPERIOD, SERIES_KEY, OBSERVATION, CONFIDENTIALITY_FLAG, UNITS, FREQ, FREQ_DESC,
           REF_AREA, REF_AREA_DESC, SECTOR, SECTOR_DESC, MRCHNT_CTGRY, MRCHNT_CTGRY_DESC,
           RMT_INTTN, RMT_INTTN_DESC, UNIT_MEASURE, UNIT_MEASURE_DESC)
}

# Cards issued (mpci): a stock --------------------------------------------------------
fn <- c(`1` = "Debit", `2` = "Credit", `3` = "Delayed debit")
ci <- bind_rows(lapply(names(fn), function(code) {
  b <- c(`1` = 6.1e6, `2` = 1.45e6, `3` = 0.12e6)[[code]]
  g <- c(`1` = 0.035, `2` = 0.01, `3` = -0.05)[[code]]
  tibble(m = months, CRD_FNCTN = code, v = round(path_level(b, g, 0.002, seasonal = FALSE)))
}))
ci <- bind_rows(ci, ci |> group_by(m) |> summarise(v = sum(v), .groups = "drop") |> mutate(CRD_FNCTN = "0"))
write_mpci <- function(d) {
  d |>
    mutate(
      REPORTING_PERIOD = period_end(m),
      SERIES_KEY = paste("PAY.M.IE.IE._Z", CRD_FNCTN, "PN", sep = "."),
      OBSERVATION_FREE = format(v, scientific = FALSE, trim = TRUE),
      CONFIDENTIALITY_FLAG = "F", FREQ = "M", FREQ_DESC = "Monthly",
      REF_AREA = "IE", REF_AREA_DESC = "Ireland", HOLDER_AREA = "IE", HOLDER_AREA_DESC = "Ireland",
      PYMNT_SCHM = "_Z", PYMNT_SCHM_DESC = "Not applicable",
      CRD_FNCTN_DESC = unname(c(fn, `0` = "All card functions")[CRD_FNCTN]),
      UNIT_MEASURE = "PN", UNIT_MEASURE_DESC = "Pure number"
    ) |>
    select(REPORTING_PERIOD, SERIES_KEY, OBSERVATION_FREE, CONFIDENTIALITY_FLAG, FREQ, FREQ_DESC,
           REF_AREA, REF_AREA_DESC, HOLDER_AREA, HOLDER_AREA_DESC, PYMNT_SCHM, PYMNT_SCHM_DESC,
           CRD_FNCTN, CRD_FNCTN_DESC, UNIT_MEASURE, UNIT_MEASURE_DESC)
}

# Write both releases ------------------------------------------------------------------
write_release <- function(dir, until, revise) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cp_r <- cp
  if (revise) {
    # Planted problem 2: May 2026 domestic online value revised up, totals with it.
    may <- cp_r$m == as.Date("2026-05-01") & cp_r$UNIT_MEASURE == "EUR" & cp_r$TYP_TRNSCTN == "CP0" &
      cp_r$INTTN_CHNNL == "_T" & cp_r$COUNT_AREA %in% c("W2", "W0") & cp_r$RMT_INTTN %in% c("R", "_T")
    cp_r$v[may] <- cp_r$v[may] + 12.3
  }
  keep <- function(d) d[d$m <= until, ]
  readr::write_csv(write_mpcp(keep(cp_r)), file.path(dir, "mpcp.csv"), na = "")
  readr::write_csv(write_mpcy(keep(cy)), file.path(dir, "mpcy.csv"), na = "")
  readr::write_csv(write_mpmc(keep(mc)), file.path(dir, "mpmc.csv"), na = "")
  readr::write_csv(write_mpci(keep(ci)), file.path(dir, "mpci.csv"), na = "")
}
write_release(file.path("tests", "fixtures", "release_2026-07"), as.Date("2026-07-01"), revise = FALSE)
write_release(file.path("tests", "fixtures", "release_2026-08"), as.Date("2026-08-01"), revise = TRUE)

# Synthetic population, proportional to the county weights. NOT CENSUS FIGURES.
pop <- tibble(county = tools::toTitleCase(tolower(names(weights))),
              population = round(5.15e6 * share_nr / sum(share_nr) * c(1)))
readr::write_csv(pop, file.path("tests", "fixtures", "county_population_synthetic.csv"))

# Boundaries for local testing: Natural Earth admin-1 (public domain), merged to 26 counties.
ne <- Sys.getenv("NATURAL_EARTH_STATES", "")
if (nzchar(ne) && file.exists(ne)) {
  e <- new.env()
  load(ne, envir = e)
  s <- sf::st_as_sf(e$states10)
  s <- s[s$admin == "Ireland", "name"]
  sf::st_write(s, file.path("tests", "fixtures", "counties_naturalearth_raw.geojson"),
               delete_dsn = TRUE, quiet = TRUE)
}
message("Fixtures written.")
