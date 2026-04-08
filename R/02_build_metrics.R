# ============================================================
# 02_build_metrics.R
# Compute Jaccard + churn metrics + baseline similarity + exposure
# Adapted for real FactSet + exemption data
# ============================================================

source("R/00_setup.R")
log_msg("Running 02_build_metrics.R ...")

panel  <- readRDS(file.path(DIR_DATA, "panel.rds"))
edges  <- readRDS(file.path(DIR_DATA, "edges.rds"))
W_norm <- readRDS(file.path(DIR_DATA, "W_norm.rds"))

setDT(panel); setDT(edges)

# ---- 1) Build firm-year supplier sets (list-column) ----
sets <- edges[, .(suppliers = list(sort(unique(supplier_id)))), by = .(firm_id, year)]
panel <- merge(panel, sets, by = c("firm_id", "year"), all.x = TRUE)

# list-safe missing detector
is_na_list <- function(x) length(x) == 1L && is.na(x[1])

miss_idx <- which(vapply(panel$suppliers, is_na_list, logical(1)))
if (length(miss_idx) > 0L) {
  panel[miss_idx, suppliers := replicate(.N, list(character(0)), simplify = FALSE)]
}

stopifnot(is.list(panel$suppliers))
setorder(panel, firm_id, year)

# ---- 2) Jaccard(t, t-1) + churn decomposition ----
setDT(panel)
setorder(panel, firm_id, year)

if ("suppliers_lag" %in% names(panel)) panel[, suppliers_lag := NULL]

# Create lag map: suppliers at year t become suppliers_lag at year t+1
lag_map <- panel[, .(
  firm_id,
  year = year + 1L,
  suppliers_lag = suppliers
)]

panel <- merge(panel, lag_map, by = c("firm_id", "year"), all.x = TRUE, sort = FALSE)

# Fill missing lag sets (first year per firm) with empty sets
miss_lag_idx <- which(vapply(panel$suppliers_lag, is_na_list, logical(1)))
if (length(miss_lag_idx) > 0L) {
  panel[miss_lag_idx, suppliers_lag := replicate(.N, list(character(0)), simplify = FALSE)]
}

stopifnot(is.list(panel$suppliers_lag))

panel[, jaccard_t_t1 := mapply(jaccard_vec, suppliers, suppliers_lag) |> as.numeric()]
panel[, churn_t_t1 := 1 - jaccard_t_t1]

panel[, n_prev := lengths(suppliers_lag)]
panel[, n_curr := lengths(suppliers)]

panel[, n_inter := mapply(function(a,b) length(intersect(a,b)), suppliers, suppliers_lag) |> as.integer()]
panel[, n_add   := mapply(function(a,b) length(setdiff(a,b)), suppliers, suppliers_lag) |> as.integer()]
panel[, n_drop  := mapply(function(a,b) length(setdiff(b,a)), suppliers, suppliers_lag) |> as.integer()]

panel[, retention_rate := safe_div(n_inter, n_prev)]
panel[, add_rate       := safe_div(n_add,   n_prev)]
panel[, drop_rate      := safe_div(n_drop,  n_prev)]

# ---- 3) Baseline similarity to pre-policy year (2017) ----
baseline_year <- 2017
base_sets <- panel[year == baseline_year, .(firm_id, suppliers_base = suppliers)]
panel <- merge(panel, base_sets, by = "firm_id", all.x = TRUE)

miss_base_idx <- which(vapply(panel$suppliers_base, is_na_list, logical(1)))
if (length(miss_base_idx) > 0L) {
  panel[miss_base_idx, suppliers_base := replicate(.N, list(character(0)), simplify = FALSE)]
}

stopifnot(is.list(panel$suppliers_base))
panel[, jaccard_to_base := mapply(jaccard_vec, suppliers, suppliers_base) |> as.numeric()]
panel[, divergence_to_base := 1 - jaccard_to_base]

# ---- 4) Neighbor exposure mapping ----
# Exposure defined on pre-policy overlap network (W_norm).
firm_ids <- sort(unique(panel$firm_id))
stopifnot(all(rownames(W_norm) %in% firm_ids))

W <- W_norm[firm_ids, firm_ids]

get_exposure <- function(vec_by_firm){
  v <- vec_by_firm[firm_ids]
  as.numeric(W %*% v)
}

# NOTE: exposure_deny and exposure_waive require waiver variables from exemption data.
# These columns must exist in panel before this block runs.
# If exemption data is not yet loaded, skip this block.

if ("deny_any" %in% names(panel) && "waiver_any" %in% names(panel)) {
  panel[, exposure_deny  := NA_real_]
  panel[, exposure_waive := NA_real_]

  for(yy in sort(unique(panel$year))){
    slice <- panel[year == yy]
    deny_vec  <- setNames(slice$deny_any, slice$firm_id)
    waive_vec <- setNames(slice$waiver_any, slice$firm_id)

    ex_deny  <- get_exposure(deny_vec)
    ex_waive <- get_exposure(waive_vec)

    panel[year == yy, exposure_deny := ex_deny]
    panel[year == yy, exposure_waive := ex_waive]
  }
  log_msg("Computed exposure_deny and exposure_waive.")
} else {
  log_msg("WARNING: waiver variables not found in panel. Skipping exposure computation.")
  log_msg("         Run 01_load_data.R with exemption data first.")
}

# ---- 5) Degree controls and treatment timing ----
# Degree controls
panel[, log_degree := log1p(n_curr)]

# Forward-fill firm financials for years outside 2019-2023 range
# (Carry last known value forward/backward within firm)
if ("log_assets" %in% names(panel)) {
  setorder(panel, firm_id, year)
  panel[, log_assets := nafill(nafill(log_assets, type = "locf"), type = "nocb"), by = firm_id]
  panel[, leverage   := nafill(nafill(leverage, type = "locf"), type = "nocb"), by = firm_id]
}

# post_tariff should already be set in 01_load_data.R
if (!"post_tariff" %in% names(panel)) {
  panel[, post_tariff := as.integer(year >= 2018)]
}

# g_year0 for staggered DiD (0 = never treated)
# Requires g_year from exemption data
if ("g_year" %in% names(panel)) {
  panel[, g_year0 := ifelse(is.na(g_year), 0L, g_year)]
} else {
  log_msg("WARNING: g_year not found. Staggered DiD timing not set.")
}

# ---- Save ----
saveRDS(panel, file = file.path(DIR_DATA, "panel_metrics.rds"))

# CSV export (flatten list columns)
panel_csv <- data.frame(panel)
for (i in 1:ncol(panel_csv)) {
  if (is.list(panel_csv[,i])) {
    panel_csv[,i] <- as.character(panel_csv[,i])
  }
}
write.csv(panel_csv, file = file.path(DIR_DATA, "panel_metrics.csv"), row.names = FALSE)

log_msg("Wrote: data/panel_metrics.rds and data/panel_metrics.csv")
