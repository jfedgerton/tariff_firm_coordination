# ============================================================
# 01_load_data.R
# Load FactSet supplier edges + USTR Section 301 exemption data
# Replaces: 01_simulate_data.R
#
# DATA SOURCES:
#   1. FactSet edge list (factset.xlsx) — 2003-2023, firm_gvkey × supplier_gvkey
#   2. FactSet financials (FactSet_19_23.dta) — 2019-2023, firm financials + SIC
#   3. USTR exemption tables (4 lists across 2 dockets):
#      - CSV Files/Tables/Requests1_Table.csv  (~10K, List 1, docket USTR-2018-0025)
#      - CSV Files/Tables/Requests2_Table.csv  (~2.9K, List 2, docket USTR-2018-0032)
#      - CSV Files/Tables/Requests3_Table.csv  (~30K, List 3, docket USTR-2019-0005)
#      - CSV Files/Tables/Requests4a_Table.csv (~8.8K, List 4a, docket USTR-2019-0017)
# ============================================================

source("R/00_setup.R")
log_msg("Running 01_load_data.R ...")

# ================================================================
# SECTION A: Load FactSet supplier relationship edge list
# ================================================================
# factset.xlsx: 2003-2023, columns: fyear, firm_gvkey, supplier_gvkey,
#   revenue_percent, firm_isin, firm_hq, supplier_isin, supplier_hq

factset_edges_path <- file.path(DIR_DATA, "factset.xlsx")
stopifnot(file.exists(factset_edges_path))

edges_raw <- as.data.table(read_excel(factset_edges_path))
log_msg("Loaded factset.xlsx:", nrow(edges_raw), "rows")

# Standardize column names
setnames(edges_raw, c("fyear", "firm_gvkey", "supplier_gvkey"),
                    c("year", "firm_id", "supplier_id"),
         skip_absent = TRUE)

edges_raw <- edges_raw[!is.na(firm_id) & !is.na(supplier_id) & !is.na(year)]
edges_raw[, firm_id     := as.character(firm_id)]
edges_raw[, supplier_id := as.character(supplier_id)]
edges_raw[, year        := as.integer(year)]

# Country from supplier_hq
edges_raw[, country := fcase(
  supplier_hq == "CN", "China",
  supplier_hq == "US", "USA",
  default = "Other"
)]

log_msg("Edge list year range:", min(edges_raw$year), "-", max(edges_raw$year))
log_msg("Unique firms:", uniqueN(edges_raw$firm_id),
        "| Unique suppliers:", uniqueN(edges_raw$supplier_id))

edges <- copy(edges_raw)

# ================================================================
# SECTION B: Load FactSet financials (.dta) for firm-level controls
# ================================================================
# FactSet_19_23.dta: 2019-2023 firm-supplier pairs with financials.
# Columns: fyear, fgvkey, ffic, fsic, fat, frevt, fdltt, fche, fxrd, ...
# We extract one row per firm-year for firm-level controls.

factset_dta_path <- file.path(DIR_DATA, "FactSet_19_23.dta")

if (file.exists(factset_dta_path)) {
  fs_dta <- as.data.table(haven::read_dta(factset_dta_path))
  log_msg("Loaded FactSet_19_23.dta:", nrow(fs_dta), "rows")

  # Extract firm-year financials (take first row per firm-year to deduplicate)
  fs_dta[, fyear_int := as.integer(format(as.Date(fyear), "%Y"))]

  firm_financials <- fs_dta[, .(
    log_assets = log(first(fat)),
    leverage   = first(fdltt) / first(fat),
    log_revenue = log(first(frevt)),
    log_rd     = log1p(first(fxrd)),
    sic_2      = first(fsic_2),
    sic_2n     = first(fsic_2n),
    ffic       = first(ffic),
    fsic       = first(fsic)
  ), by = .(firm_id = as.character(fgvkey), year = fyear_int)]

  # Clean leverage
  firm_financials[is.infinite(leverage) | leverage < 0, leverage := NA_real_]
  firm_financials[leverage > 1, leverage := 1]
  firm_financials[is.infinite(log_assets), log_assets := NA_real_]

  log_msg("Built firm_financials:", nrow(firm_financials), "firm-years")
} else {
  log_msg("WARNING: FactSet_19_23.dta not found. Skipping firm financials.")
  firm_financials <- NULL
}

# ================================================================
# SECTION C: Build firm and supplier reference tables
# ================================================================

# Firms from edge list
firms <- edges[, .(
  firm_hq    = first(firm_hq),
  first_year = min(year),
  last_year  = max(year),
  avg_degree = .N / uniqueN(year)
), by = firm_id]

# Merge industry from financials if available
if (!is.null(firm_financials)) {
  firm_ind <- firm_financials[, .(industry = first(na.omit(sic_2n)),
                                   ffic = first(na.omit(ffic))), by = firm_id]
  firms <- merge(firms, firm_ind, by = "firm_id", all.x = TRUE)
}

# Suppliers from edge list
suppliers <- edges[, .(
  supplier_hq = first(supplier_hq),
  country     = first(country)
), by = supplier_id]

log_msg("Firm table:", nrow(firms), "firms")
log_msg("Supplier table:", nrow(suppliers), "suppliers")

# ================================================================
# SECTION D: Load and harmonize USTR exemption tables
# ================================================================
# Lists 1-2 schema: Doc Type, Request Doc-ID, Organization Name,
#                   10-Digit HTS, Date Posted, Response Closes, Reply Closes, Stages
# Lists 3-4 schema: Request, Org, Status, HTSUS, Product, Post Date, Close Date

tables_dir <- file.path(ROOT, "CSV Files", "Tables")

load_list12 <- function(path, list_num){
  dt <- fread(path, na.strings = c("", "NA"))
  setnames(dt, c("Request Doc-ID", "Organization Name", "10-Digit HTS", "Stages", "Date Posted"),
               c("request_id", "org_name", "hts_code", "status", "date_posted"),
           skip_absent = TRUE)
  dt[, list_num := list_num]
  dt[, .(request_id, org_name, hts_code, status, date_posted, list_num)]
}

load_list34 <- function(path, list_num){
  dt <- fread(path, na.strings = c("", "NA"))
  setnames(dt, c("Request", "Org", "Status", "HTSUS", "Post Date"),
               c("request_id", "org_name", "status", "hts_code", "date_posted"),
           skip_absent = TRUE)
  dt[, list_num := list_num]
  dt[, .(request_id, org_name, hts_code, status, date_posted, list_num)]
}

req_files <- list(
  list(path = file.path(tables_dir, "Requests1_Table.csv"), loader = load_list12, list_num = 1L),
  list(path = file.path(tables_dir, "Requests2_Table.csv"), loader = load_list12, list_num = 2L),
  list(path = file.path(tables_dir, "Requests3_Table.csv"), loader = load_list34, list_num = 3L),
  list(path = file.path(tables_dir, "Requests4a_Table.csv"), loader = load_list34, list_num = 4L)
)

requests_list <- list()
for (rf in req_files) {
  if (file.exists(rf$path)) {
    requests_list[[length(requests_list) + 1]] <- rf$loader(rf$path, rf$list_num)
    log_msg("Loaded", basename(rf$path), ":", nrow(requests_list[[length(requests_list)]]), "rows")
  } else {
    log_msg("WARNING: Not found:", rf$path)
  }
}

requests <- rbindlist(requests_list, use.names = TRUE, fill = TRUE)
log_msg("Total exemption requests:", nrow(requests))

# Clean status
requests[, status := trimws(status)]
requests[, grant := as.integer(tolower(status) == "granted")]

# Parse decision year from date_posted
# Lists 1-2 store dates as Excel serial numbers; Lists 3-4 as text dates
requests[, date_posted := as.character(date_posted)]

# Try parsing as Excel serial number first, then as text date
requests[, decision_date := as.Date(NA)]
# Excel serial: 5-digit numbers
requests[grepl("^\\d{5}$", date_posted),
         decision_date := as.Date(as.numeric(date_posted), origin = "1899-12-30")]
# Text date formats
requests[is.na(decision_date) & !is.na(date_posted),
         decision_date := as.Date(date_posted, format = "%d-%b-%y")]
requests[is.na(decision_date) & !is.na(date_posted),
         decision_date := as.Date(date_posted, format = "%Y-%m-%dT%H:%M")]
requests[is.na(decision_date) & !is.na(date_posted),
         decision_date := as.Date(date_posted, format = "%m/%d/%Y")]

requests[, decision_year := as.integer(format(decision_date, "%Y"))]

log_msg("Decision year range:", min(requests$decision_year, na.rm=TRUE), "-",
        max(requests$decision_year, na.rm=TRUE))
log_msg("Status breakdown:")
print(requests[, .N, by = status][order(-N)])

# Clean organization names for matching
requests[, org_clean := str_to_upper(trimws(org_name))]
requests[, org_clean := str_replace_all(org_clean, "[[:punct:]]", " ")]
requests[, org_clean := str_squish(org_clean)]

# ================================================================
# SECTION E: Match exemption orgs to FactSet firm GVKEYs
# ================================================================
# This is the critical merge step. The exemption data uses company names;
# FactSet uses numeric GVKEYs. We need a name-to-GVKEY crosswalk.
#
# Strategy:
#   1. Build a firm name lookup from FactSet .dta (if available) or edge list
#   2. Use fuzzy string matching (stringdist) for unmatched names
#   3. Save the crosswalk for manual review/correction
#
# NOTE: This section will likely need manual review. Check crosswalk output.

# If FactSet .dta is available, it may have company names in labels or we
# can try to use ISIN-based matching. For now, we create a placeholder
# crosswalk file that should be manually completed.

crosswalk_path <- file.path(DIR_DATA, "org_gvkey_crosswalk.csv")

if (file.exists(crosswalk_path)) {
  # Use existing manually-reviewed crosswalk
  crosswalk <- fread(crosswalk_path)
  log_msg("Loaded existing crosswalk:", nrow(crosswalk), "entries")
} else {
  # Create a template crosswalk from unique org names
  unique_orgs <- requests[, .(n_requests = .N), by = org_name][order(-n_requests)]
  unique_orgs[, firm_id := NA_character_]  # to be filled manually or by fuzzy match
  unique_orgs[, match_confidence := NA_character_]

  fwrite(unique_orgs, crosswalk_path)
  log_msg("Created crosswalk template at:", crosswalk_path)
  log_msg("  ", nrow(unique_orgs), "unique organizations to match")
  log_msg("  Fill in firm_id (GVKEY) column and re-run this script.")
}

# Merge crosswalk onto requests
if (exists("crosswalk") && "firm_id" %in% names(crosswalk)) {
  requests <- merge(requests, crosswalk[, .(org_name, firm_id)],
                    by = "org_name", all.x = TRUE)
  matched <- sum(!is.na(requests$firm_id))
  log_msg("Matched", matched, "of", nrow(requests), "requests to GVKEYs",
          sprintf("(%.1f%%)", 100 * matched / nrow(requests)))
} else {
  requests[, firm_id := NA_character_]
  log_msg("WARNING: No crosswalk available. Exemption-FactSet merge pending.")
}

# ================================================================
# SECTION F: Build firm-year panel
# ================================================================

# Panel skeleton: all firm-year combinations in FactSet edges
panel <- CJ(firm_id = unique(edges$firm_id), year = sort(unique(edges$year)))

# Firm-year supplier geography stats
firm_year_stats <- edges[, .(
  degree      = .N,
  share_china = mean(country == "China", na.rm = TRUE),
  share_usa   = mean(country == "USA", na.rm = TRUE)
), by = .(firm_id, year)]

panel <- merge(panel, firm_year_stats, by = c("firm_id", "year"), all.x = TRUE)
panel[is.na(degree), degree := 0L]

# Merge firm-level attributes
panel <- merge(panel, firms[, .(firm_id, firm_hq)], by = "firm_id", all.x = TRUE)

# Merge firm financials (if available)
if (!is.null(firm_financials)) {
  panel <- merge(panel, firm_financials[, .(firm_id, year, log_assets, leverage,
                                             log_revenue, sic_2n, industry = sic_2n)],
                 by = c("firm_id", "year"), all.x = TRUE)
  log_msg("Merged firm financials. Non-NA log_assets:", sum(!is.na(panel$log_assets)))
} else {
  panel[, log_assets := NA_real_]
  panel[, leverage := NA_real_]
  panel[, industry := NA_character_]
}

# ================================================================
# SECTION G: Aggregate exemption data to firm-year level
# ================================================================
# Only runs if crosswalk is available

if (any(!is.na(requests$firm_id))) {
  req_matched <- requests[!is.na(firm_id)]
  log_msg("Building exemption panel from", nrow(req_matched), "matched requests")

  # Firm-level event years
  req_firm <- req_matched[, .(
    first_grant_year = if(any(grant == 1)) min(decision_year[grant == 1], na.rm=TRUE) else NA_integer_,
    first_deny_year  = if(any(grant == 0)) min(decision_year[grant == 0], na.rm=TRUE) else NA_integer_,
    ever_grant       = as.integer(any(grant == 1)),
    ever_deny        = as.integer(any(grant == 0)),
    total_requests   = .N
  ), by = firm_id]

  panel <- merge(panel, req_firm, by = "firm_id", all.x = TRUE)

  # Fill NAs for firms with no exemption requests
  panel[is.na(ever_grant), ever_grant := 0L]
  panel[is.na(ever_deny), ever_deny := 0L]
  panel[is.na(total_requests), total_requests := 0L]

  # Cumulative waiver intensity by year
  setkey(req_matched, firm_id, decision_year)
  setkey(panel, firm_id, year)

  tmp <- req_matched[panel,
                     on = .(firm_id, decision_year <= year),
                     allow.cartesian = TRUE,
                     .(firm_id, year = i.year, grant)]

  # Remove rows where join produced NAs (no decisions yet for this firm-year)
  tmp <- tmp[!is.na(grant)]

  if (nrow(tmp) > 0) {
    panel_long <- tmp[, .(
      n_decided = .N,
      n_grant   = sum(grant == 1, na.rm = TRUE),
      n_deny    = sum(grant == 0, na.rm = TRUE)
    ), by = .(firm_id, year)]

    panel <- merge(panel, panel_long, by = c("firm_id", "year"), all.x = TRUE)
  }

  panel[is.na(n_decided), `:=`(n_decided = 0L, n_grant = 0L, n_deny = 0L)]

  panel[, grant_share := ifelse(n_decided == 0, 0, n_grant / n_decided)]
  panel[, waiver_any  := as.integer(n_grant > 0)]
  panel[, deny_any    := as.integer(n_deny > 0)]

  # Staggered adoption timing
  panel[, g_year := first_grant_year]
  panel[, post_grant := as.integer(!is.na(g_year) & year >= g_year)]

  log_msg("Exemption variables built. Firms with any grant:", sum(panel$ever_grant == 1, na.rm=TRUE) / uniqueN(panel$year))
} else {
  log_msg("WARNING: No exemption-FactSet matches. Skipping exemption panel construction.")
  log_msg("         Complete the crosswalk at:", crosswalk_path)
}

# ================================================================
# SECTION H: Define pre-policy period
# ================================================================
PRE_POLICY_END <- 2017
panel[, post_tariff := as.integer(year >= 2018)]

# ================================================================
# SECTION I: Build pre-policy firm-firm overlap network (W)
# ================================================================
pre_edges <- edges[year == PRE_POLICY_END]
pre_sets  <- split(pre_edges$supplier_id, pre_edges$firm_id)

firm_ids <- sort(unique(panel$firm_id))
n_firms  <- length(firm_ids)

log_msg("Building pre-policy overlap matrix for", n_firms, "firms...")
log_msg("  (This may take a while for large N. Consider subsetting if N > 2000)")

# For very large N, use sparse computation
if (n_firms > 2000) {
  log_msg("  Large N detected. Using sparse Jaccard computation...")
  # Build incidence matrix approach for speed
  all_suppliers <- unique(unlist(pre_sets))
  n_sup <- length(all_suppliers)
  sup_idx <- setNames(seq_along(all_suppliers), all_suppliers)

  # Sparse incidence matrix: firms × suppliers
  ii <- jj <- integer(0)
  for (f in seq_along(firm_ids)) {
    s <- pre_sets[[firm_ids[f]]]
    if (!is.null(s) && length(s) > 0) {
      ii <- c(ii, rep(f, length(s)))
      jj <- c(jj, sup_idx[s])
    }
  }
  Inc <- sparseMatrix(i = ii, j = jj, x = 1, dims = c(n_firms, n_sup),
                      dimnames = list(firm_ids, all_suppliers))

  # Jaccard from incidence: J(i,j) = |A∩B| / |A∪B| = dot(i,j) / (|A|+|B|-dot(i,j))
  dot_mat <- tcrossprod(Inc)
  deg <- Matrix::rowSums(Inc)
  deg_sum <- outer(deg, deg, "+")

  W <- as.matrix(dot_mat / (deg_sum - dot_mat))
  W[is.nan(W)] <- 0
  diag(W) <- 0
  dimnames(W) <- list(firm_ids, firm_ids)
} else {
  W <- matrix(0, nrow = n_firms, ncol = n_firms, dimnames = list(firm_ids, firm_ids))
  for(i in seq_len(n_firms)){
    a <- pre_sets[[firm_ids[i]]]
    if(is.null(a)) a <- character(0)
    for(j in seq_len(n_firms)){
      if(i == j) next
      b <- pre_sets[[firm_ids[j]]]
      if(is.null(b)) b <- character(0)
      W[i,j] <- jaccard_vec(a, b)
    }
  }
}

# Threshold small overlaps
W[W < 0.05] <- 0
diag(W) <- 0

# Row-normalize
row_sums <- rowSums(W)
W_norm <- W
for(i in seq_len(n_firms)){
  if(row_sums[i] > 0) W_norm[i,] <- W[i,] / row_sums[i]
}

log_msg("W_norm built. Non-zero entries:", sum(W_norm > 0))

# ================================================================
# SECTION J: Save outputs
# ================================================================
saveRDS(panel,     file = file.path(DIR_DATA, "panel.rds"))
saveRDS(edges,     file = file.path(DIR_DATA, "edges.rds"))
saveRDS(firms,     file = file.path(DIR_DATA, "firms.rds"))
saveRDS(suppliers, file = file.path(DIR_DATA, "suppliers.rds"))
saveRDS(W_norm,    file = file.path(DIR_DATA, "W_norm.rds"))
saveRDS(requests,  file = file.path(DIR_DATA, "requests.rds"))

log_msg("01_load_data.R complete.")
if (!any(!is.na(requests$firm_id))) {
  log_msg("NEXT STEP: Complete org_gvkey_crosswalk.csv and re-run.")
}
