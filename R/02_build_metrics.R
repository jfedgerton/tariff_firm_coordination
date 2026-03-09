# ============================================================
# 02_build_metrics.R
# Compute Jaccard + churn metrics + baseline similarity + exposure
# ============================================================

source("R/00_setup.R")
log_msg("Running 02_build_metrics.R ...")

panel <- readRDS(file.path(DIR_DATA, "sim_panel.rds"))
edges <- readRDS(file.path(DIR_DATA, "sim_edges.rds"))
W_norm <- readRDS(file.path(DIR_DATA, "sim_W_norm.rds"))

setDT(panel); setDT(edges)

# ---- 1) Build firm-year supplier sets (list-column) ----
sets <- edges[, .(suppliers = list(sort(unique(supplier_id)))), by = .(firm_id, year)]
panel <- merge(panel, sets, by = c("firm_id","year"), all.x = TRUE)

# list-safe missing detector: catches both NULL (from data.table merge) and NA
is_missing_list <- function(x) is.null(x) || (length(x) == 1L && is.na(x[1]))

miss_idx <- which(vapply(panel$suppliers, is_missing_list, logical(1)))
if (length(miss_idx) > 0L) {
  panel[miss_idx, suppliers := replicate(.N, list(character(0)), simplify = FALSE)]
}

stopifnot(is.list(panel$suppliers))

setorder(panel, firm_id, year)

# ---- 2) Jaccard(t, t-1) + churn decomposition ----
# 0) Make sure panel is a data.table
setDT(panel)

# 1) Ensure proper ordering
setorder(panel, firm_id, year)

# ---- 2) Build suppliers_lag via self-merge (list-safe, no grouped :=) ----

# drop if exists
if ("suppliers_lag" %in% names(panel)) panel[, suppliers_lag := NULL]

# create lag map: suppliers at year t become suppliers_lag at year t+1
lag_map <- panel[, .(
  firm_id,
  year = year + 1L,
  suppliers_lag = suppliers
)]

# merge lagged suppliers back onto panel
panel <- merge(panel, lag_map, by = c("firm_id", "year"), all.x = TRUE, sort = FALSE)

# fill missing lag sets (first year per firm) with empty sets
# data.table merge fills missing list entries with NULL (length 0), not NA
is_missing_list <- function(x) is.null(x) || (length(x) == 1L && is.na(x[1]))

miss_lag_idx <- which(vapply(panel$suppliers_lag, is_missing_list, logical(1)))

# Track which rows have no genuine lag (first year per firm)
panel[, has_genuine_lag := TRUE]
if (length(miss_lag_idx) > 0L) {
  panel[miss_lag_idx, has_genuine_lag := FALSE]
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

# Set metrics to NA for first year per firm (no genuine lag to compare against)
lag_cols <- c("jaccard_t_t1", "churn_t_t1", "retention_rate", "add_rate", "drop_rate")
panel[has_genuine_lag == FALSE, (lag_cols) := NA_real_]

# ---- 3) Baseline similarity to a pre-policy year (e.g., 2017) ----
baseline_year <- 2017
base_sets <- panel[year == baseline_year, .(firm_id, suppliers_base = suppliers)]
panel <- merge(panel, base_sets, by = "firm_id", all.x = TRUE)

# list-safe NA fill for suppliers_base (DO NOT use is.na() on list columns)
miss_base_idx <- which(vapply(panel$suppliers_base, is_missing_list, logical(1)))
if (length(miss_base_idx) > 0L) {
  panel[miss_base_idx, suppliers_base := replicate(.N, list(character(0)), simplify = FALSE)]
}

stopifnot(is.list(panel$suppliers_base))
panel[, jaccard_to_base := mapply(jaccard_vec, suppliers, suppliers_base) |> as.numeric()]
panel[, divergence_to_base := 1 - jaccard_to_base]

# ---- 3b) Country-specific Jaccard decomposition ----
# Split supplier sets by country for Chinese vs non-Chinese churn
edges_china    <- edges[country == "China"]
edges_nonchina <- edges[country != "China"]

sets_china    <- edges_china[, .(sup_china = list(sort(unique(supplier_id)))), by = .(firm_id, year)]
sets_nonchina <- edges_nonchina[, .(sup_nonchina = list(sort(unique(supplier_id)))), by = .(firm_id, year)]

panel <- merge(panel, sets_china,    by = c("firm_id", "year"), all.x = TRUE)
panel <- merge(panel, sets_nonchina, by = c("firm_id", "year"), all.x = TRUE)

# Fill missing country sets with empty lists
for(col in c("sup_china", "sup_nonchina")){
  miss <- which(vapply(panel[[col]], is_missing_list, logical(1)))
  if(length(miss) > 0L) panel[miss, (col) := replicate(.N, list(character(0)), simplify = FALSE)]
}

# Build lagged country sets
lag_china <- panel[, .(firm_id, year = year + 1L, sup_china_lag = sup_china)]
lag_nonch <- panel[, .(firm_id, year = year + 1L, sup_nonchina_lag = sup_nonchina)]

panel <- merge(panel, lag_china, by = c("firm_id", "year"), all.x = TRUE, sort = FALSE)
panel <- merge(panel, lag_nonch, by = c("firm_id", "year"), all.x = TRUE, sort = FALSE)

for(col in c("sup_china_lag", "sup_nonchina_lag")){
  miss <- which(vapply(panel[[col]], is_missing_list, logical(1)))
  if(length(miss) > 0L) panel[miss, (col) := replicate(.N, list(character(0)), simplify = FALSE)]
}

panel[, jaccard_china    := mapply(jaccard_vec, sup_china, sup_china_lag) |> as.numeric()]
panel[, jaccard_nonchina := mapply(jaccard_vec, sup_nonchina, sup_nonchina_lag) |> as.numeric()]
panel[, churn_china      := 1 - jaccard_china]
panel[, churn_nonchina   := 1 - jaccard_nonchina]
panel[has_genuine_lag == FALSE, c("jaccard_china", "jaccard_nonchina",
                                   "churn_china", "churn_nonchina") := NA_real_]

# ---- 3c) Edge-level survival ----
# For each firm-year, compute fraction of t-1 edges that survive to t
edge_surv <- edges[, .(firm_id, year, supplier_id)]
edge_surv_lag <- edges[, .(firm_id, year = year + 1L, supplier_id)]
setnames(edge_surv_lag, "supplier_id", "supplier_lag")

# Merge: for each firm-year, join current and lagged edges
edge_surv_merged <- merge(edge_surv_lag, edge_surv,
                          by = c("firm_id", "year"), allow.cartesian = TRUE)
edge_surv_merged[, survived := as.integer(supplier_lag == supplier_id)]

# Compute edge survival rate per firm-year
edge_survival_rate <- edge_surv_merged[, .(
  edge_survival = mean(survived),
  n_edges_prev  = uniqueN(supplier_lag)
), by = .(firm_id, year)]

panel <- merge(panel, edge_survival_rate, by = c("firm_id", "year"), all.x = TRUE)

# ---- 4) Neighbor exposure mapping to handle spillovers ----
# Exposure defined on pre-policy overlap network (W_norm).
firm_ids <- sort(unique(panel$firm_id))
stopifnot(all(rownames(W_norm) %in% firm_ids))

# Ensure order alignment
W <- W_norm[firm_ids, firm_ids]

get_exposure <- function(vec_by_firm){
  v <- vec_by_firm[firm_ids]
  as.numeric(W %*% v)
}

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

# ---- 5) Minimal analysis-ready outcomes & covariates ----
# Degree controls: supplier counts can mechanically affect Jaccard
panel[, log_degree := log1p(n_curr)]

# Post common tariff shock (for illustration)
panel[, post_tariff := as.integer(year >= 2018)]

# "Treatment timing" for staggered DiD: first grant year
# g_year already exists; keep as-is
# For never-treated, did::att_gt expects gname=0 for never-treated
panel[, g_year0 := ifelse(is.na(g_year), 0L, g_year)]

saveRDS(panel, file = file.path(DIR_DATA, "panel_metrics.rds"))
panel_csv <- data.frame(panel)    
for (i in 1:ncol(panel_csv))
{
  if (is.list(panel_csv[,i]))
  {
    panel_csv[,i] <- as.character(panel_csv[,i]) 
  }
}
write.csv(panel_csv, file = file.path(DIR_DATA, "panel_metrics.csv"))

log_msg("Wrote: data/panel_metrics.rds")
