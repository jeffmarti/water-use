# ============================================================
# download_nwaa_wa.R
#
# One-time (re-runnable) bulk download of the USGS NWAA water-use
# data that app.R needs, saved to nwaa_wa_2010_2020.rds so the
# Shiny app never has to call the API at runtime.
#
# Why by HUC-6: each uncached API request costs ~10-30 s no matter
# how much it returns, so a few large requests beat many small ones.
# Querying by HUC-6 (rather than stateCd:WA) keeps every HUC-12 of
# each HUC-8 in wa_huc8_lookup.csv, including the out-of-state
# parts of border watersheds, matching what a huc8: query returns.
#
# Run from the project folder:  Rscript download_nwaa_wa.R
# Re-run if USGS revises the data or you change the years/variables.
# Pages are cached in nwaa_download_cache/ so an interrupted run
# resumes; delete that folder to force a completely fresh download.
# ============================================================

suppressPackageStartupMessages({
  library(httr)
  library(readr)
  library(dplyr)
  library(purrr)
})

BASE_URL   <- "https://api.water.usgs.gov/nwaa-data/data"
START_YEAR <- 2010
END_YEAR   <- 2020
PAGE_SIZE  <- 600          # API maximum (counts HUC-12s, not rows)
OUT_FILE   <- "nwaa_wa_2010_2020.rds"
CACHE_DIR  <- "nwaa_download_cache"

# Variables grouped by model: one request can carry all of a model's variables
models <- list(
  "wu-public-supply-wd" = c("pswdtot", "pswdgw", "pswdsw"),
  "wu-irrigation-wd"    = c("irrwdtot", "irrwdsw"),
  "wu-irrigation-cu"    = c("irrcutot")
)

huc8s <- read.csv("wa_huc8_lookup.csv", colClasses = "character")$huc8
huc6s <- sort(unique(substr(huc8s, 1, 6)))
dir.create(CACHE_DIR, showWarnings = FALSE)

# Fetch one page (with retries); returns NULL when skip is past the end
fetch_page <- function(model, vars, huc6, timeres, skip) {
  cache_file <- file.path(
    CACHE_DIR, sprintf("%s_%s_%s_%05d.csv", model, huc6, timeres, skip)
  )
  if (file.exists(cache_file)) {
    txt <- read_file(cache_file)
  } else {
    monthly <- timeres == "monthly"
    query <- list(
      model     = model,
      variable  = paste(vars, collapse = ","),
      location  = paste0("huc6:", huc6),
      timeres   = timeres,
      startdate = if (monthly) paste0(START_YEAR, "-01") else START_YEAR,
      enddate   = if (monthly) paste0(END_YEAR,   "-12") else END_YEAR,
      format    = "csv",
      limit     = PAGE_SIZE,
      skip      = skip
    )
    resp <- RETRY("GET", BASE_URL, query = query, timeout(600),
                  times = 5, pause_base = 5, pause_cap = 60,
                  terminate_on = c(400, 404, 422), quiet = TRUE)
    if (status_code(resp) == 400 && skip > 0) return(NULL)  # past last page
    stop_for_status(resp)
    txt <- content(resp, "text", encoding = "UTF-8")
    write_file(txt, cache_file)
  }
  # IDs and year_month stay text; year and the value columns are numeric
  read_csv(I(txt), show_col_types = FALSE,
           col_types = cols(.default = col_character())) %>%
    mutate(across(-any_of(c("huc12_id", "year_month")), as.numeric))
}

# Fetch every page for one model x HUC-6 x time resolution
fetch_all <- function(model, vars, huc6, timeres) {
  out  <- list()
  skip <- 0
  repeat {
    t0   <- Sys.time()
    page <- fetch_page(model, vars, huc6, timeres, skip)
    if (is.null(page) || nrow(page) == 0) break
    n_huc12 <- n_distinct(page$huc12_id)
    message(sprintf("  %-20s huc6:%s %-8s skip=%-5d %4d HUC12s  %5.1fs",
                    model, huc6, timeres, skip, n_huc12,
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    out[[length(out) + 1]] <- page
    if (n_huc12 < PAGE_SIZE) break
    skip <- skip + PAGE_SIZE
  }
  bind_rows(out)
}

# Download one time resolution -> one wide table (a column per variable)
download_timeres <- function(timeres) {
  time_col <- if (timeres == "monthly") "year_month" else "year"
  per_model <- imap(models, function(vars, model) {
    map_dfr(huc6s, ~ fetch_all(model, vars, .x, timeres)) %>%
      filter(substr(huc12_id, 1, 8) %in% huc8s) %>%
      distinct(huc12_id, .data[[time_col]], .keep_all = TRUE)
  })
  reduce(per_model, full_join, by = c("huc12_id", time_col)) %>%
    arrange(huc12_id, .data[[time_col]])
}

message("HUC-6 basins to download: ", paste(huc6s, collapse = ", "))
message("---- Annual (calendar year) ----")
annual  <- download_timeres("annualcy")
message("---- Monthly ----")
monthly <- download_timeres("monthly")

# Sanity checks
expected_vars <- paste0(unlist(models, use.names = FALSE), "_mgd")
stopifnot(all(expected_vars %in% names(annual)),
          all(expected_vars %in% names(monthly)))
missing_huc8 <- setdiff(huc8s, unique(substr(monthly$huc12_id, 1, 8)))
if (length(missing_huc8) > 0) {
  warning("No data returned for HUC-8(s): ", paste(missing_huc8, collapse = ", "))
}

saveRDS(
  list(
    annual     = annual,
    monthly    = monthly,
    years      = c(START_YEAR, END_YEAR),
    downloaded = Sys.time(),
    source     = BASE_URL
  ),
  OUT_FILE
)

message(sprintf(
  "Saved %s: %d HUC-12s in %d HUC-8s; %s annual rows, %s monthly rows (%.1f MB)",
  OUT_FILE, n_distinct(monthly$huc12_id),
  n_distinct(substr(monthly$huc12_id, 1, 8)),
  format(nrow(annual), big.mark = ","), format(nrow(monthly), big.mark = ","),
  file.size(OUT_FILE) / 1e6
))
