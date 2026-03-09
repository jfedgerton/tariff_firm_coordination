# ============================================================
# 01_simulate_data.R
# Simulate firm–supplier panel + staggered waiver decisions
# ============================================================

source("R/00_setup.R")
log_msg("Running 01_simulate_data.R ...")

simulate_firm_supplier_panel <- function(
  n_firms = 300,
  n_suppliers = 2500,
  years = 2014:2022,
  pre_policy_end = 2017,
  seed = 123
){
  set.seed(seed)

  years <- sort(unique(years))
  stopifnot(pre_policy_end %in% years)

  # -------------------------
  # 1) Firms + suppliers
  # -------------------------
  firms <- data.table(
    firm_id  = sprintf("F%04d", 1:n_firms),
    industry = sample(paste0("IND", 1:10), n_firms, replace = TRUE),
    log_assets = rnorm(n_firms, mean = 10, sd = 1),
    leverage   = pmin(pmax(rbeta(n_firms, 2, 5), 0.01), 0.99)
  )
  firms[, assets := exp(log_assets)]
  firms[, size_z := as.numeric(scale(log_assets))]
  firms[, degree0 := pmax(6, rpois(n_firms, lambda = 22) + 4)]

  suppliers <- data.table(
    supplier_id = sprintf("S%05d", 1:n_suppliers),
    sup_industry = sample(paste0("IND", 1:10), n_suppliers, replace = TRUE),
    country = sample(c("China", "USA", "Other"), n_suppliers, replace = TRUE,
                     prob = c(0.25, 0.35, 0.40))
  )

  # Industry-specific supplier pools (induces overlap within industries)
  global_ids <- suppliers[sample(.N, size = round(0.20 * n_suppliers)), supplier_id]
  pool <- suppliers[, .(supplier_id), by = sup_industry]
  pool <- pool[, .(supplier_id = unique(c(supplier_id, global_ids))), by = sup_industry]
  setnames(pool, "sup_industry", "industry")

  pool_china    <- merge(pool, suppliers[, .(supplier_id, country)], by = "supplier_id")[country == "China"]
  pool_nonchina <- merge(pool, suppliers[, .(supplier_id, country)], by = "supplier_id")[country != "China"]

  # -------------------------
  # 2) Waiver requests (multiple per firm)
  # -------------------------
  # ~20% of firms never apply for waivers (creates cleaner control group)
  firms[, applied := rbinom(.N, 1, prob = 0.80)]

  # Decision timing roughly 2018-2020 (matches the proposal narrative)
  firms_for_join <- copy(firms[applied == 1L])
  setkey(firms_for_join, firm_id)

  requests <- firms_for_join[, {
    n_req <- rpois(1, lambda = 3) + 1
    # grant probability depends on size + (some) industry (selection into treatment)
    p_grant <- plogis(-0.3 + 0.6 * size_z + ifelse(industry %in% c("IND9","IND10"), -0.4, 0))
    data.table(
      request_id = sprintf("%s_R%02d", firm_id, 1:n_req),
      list       = sample(1:4, n_req, replace = TRUE),
      decision_year = sample(2018:2020, n_req, replace = TRUE, prob = c(0.45, 0.35, 0.20)),
      grant      = rbinom(n_req, 1, prob = p_grant)
    )
  }, by = .(firm_id, industry, size_z)]

  # Comment process / sensitivity (simulate at request-level; later aggregated)
  requests[, comment_count := rpois(.N, lambda = exp(-1.6 + 0.30 * size_z + 0.15 * (industry == "IND3")))]
  requests[, support_share := rbeta(.N, shape1 = 2.2, shape2 = 2.2)]
  requests[, support_count := round(comment_count * support_share)]
  requests[, oppose_count  := pmax(0L, comment_count - support_count)]

  # -------------------------
  # 3) Firm-year panel with waiver variables
  # -------------------------
  panel <- CJ(firm_id = firms$firm_id, year = years)
  panel <- merge(panel, firms[, .(firm_id, industry, log_assets, assets, leverage, degree0, size_z)],
                 by = "firm_id", all.x = TRUE)

  setorder(panel, firm_id, year)
  
  # mild within-firm evolution (random walk-ish)
  panel[, log_assets := log_assets + cumsum(rnorm(.N, 0, 0.05)), by = firm_id]
  panel[, assets := exp(log_assets)]
  
  panel[, leverage := leverage + cumsum(rnorm(.N, 0, 0.02)), by = firm_id]
  panel[, leverage := pmin(pmax(leverage, 0.01), 0.99)]
  
  # allow degree target to drift over time (used later in supplier simulation)
  panel[, degree0_t := pmax(1L, round(degree0 * exp(cumsum(rnorm(.N, 0, 0.03))))), by = firm_id]
  
  # Firm-level "event years"
  req_firm <- requests[, .(
    first_grant_year = if(any(grant == 1)) min(decision_year[grant == 1]) else NA_integer_,
    first_deny_year  = if(any(grant == 0)) min(decision_year[grant == 0]) else NA_integer_,
    ever_grant = as.integer(any(grant == 1)),
    ever_deny  = as.integer(any(grant == 0)),
    total_requests = .N,
    total_comments = sum(comment_count, na.rm = TRUE)
  ), by = firm_id]

  panel <- merge(panel, req_firm, by = "firm_id", all.x = TRUE)

  # Cumulative waiver intensity by year (share granted among decided requests)
  setkey(requests, firm_id, decision_year)
  setkey(panel, firm_id, year)

  # Non-equi join: attach all requests decided up to each year, then aggregate
  tmp <- requests[panel,
                  on = .(firm_id, decision_year <= year),
                  allow.cartesian = TRUE,
                  .(
                    firm_id,
                    year = i.year,
                    grant,
                    comment_count,
                    support_count,
                    oppose_count
                  )
  ]
  
  panel_long <- tmp[, .(
    n_decided    = .N,
    n_grant      = sum(grant == 1, na.rm = TRUE),
    n_deny       = sum(grant == 0, na.rm = TRUE),
    comments_cum = sum(comment_count, na.rm = TRUE),
    support_cum  = sum(support_count, na.rm = TRUE),
    oppose_cum   = sum(oppose_count, na.rm = TRUE)
  ), by = .(firm_id, year)]

  panel <- merge(panel, panel_long, by = c("firm_id","year"), all.x = TRUE)

  # Replace NAs for pre-decision years and non-applicant firms with zeros
  panel[is.na(n_decided), `:=`(n_decided = 0L, n_grant = 0L, n_deny = 0L,
                              comments_cum = 0L, support_cum = 0L, oppose_cum = 0L)]
  # Non-applicant firms: fill missing aggregate fields
  panel[is.na(ever_grant), `:=`(ever_grant = 0L, ever_deny = 0L,
                                total_requests = 0L, total_comments = 0L)]

  panel[, grant_share := ifelse(n_decided == 0, 0, n_grant / n_decided)]
  panel[, waiver_any  := as.integer(n_grant > 0)]
  panel[, deny_any    := as.integer(n_deny > 0)]
  panel[, sensitivity_any := as.integer(total_comments > 0)]  # static
  panel[, sensitivity_cum := as.integer(comments_cum > 0)]    # time-varying

  # Define "post" relative to firm's first grant year (staggered adoption)
  panel[, g_year := first_grant_year]  # event-year for waiver receipt
  panel[, post_grant := as.integer(!is.na(g_year) & year >= g_year)]

  # -------------------------
  # 4) Simulate supplier sets over time (bipartite edges)
  # -------------------------
  # Key fix:
  #   Build suppliers recursively with explicit persistence (keep fraction),
  #   so Jaccard(t,t-1) is not degenerate (all zeros).
  #
  # Degree target is time-varying (degree0_t) to allow network size changes.
  # Post-policy persistence depends on waiver/denial and spillovers.
  
  firm_ids <- firms$firm_id
  year0 <- years[1]
  
  # --- utilities (keep simple and deterministic) ---
  sample_from_pool <- function(ind, k, exclude = character(0)){
    cand <- pool[industry == ind, supplier_id]
    cand <- setdiff(cand, exclude)
    if(k <= 0 || length(cand) == 0) return(character(0))
    sample(cand, size = min(k, length(cand)), replace = FALSE)
  }
  
  sample_new_suppliers <- function(ind, n_add, existing, p_china){
    if(n_add <= 0) return(character(0))
    
    china_cand <- pool_china[industry == ind, supplier_id]
    non_cand   <- pool_nonchina[industry == ind, supplier_id]
    
    china_cand <- setdiff(china_cand, existing)
    non_cand   <- setdiff(non_cand, existing)
    
    n_china <- rbinom(1, size = n_add, prob = p_china)
    n_non   <- n_add - n_china
    
    add_ids <- character(0)
    if(n_china > 0 && length(china_cand) > 0){
      add_ids <- c(add_ids, sample(china_cand, size = min(n_china, length(china_cand)), replace = FALSE))
    }
    if(n_non > 0 && length(non_cand) > 0){
      add_ids <- c(add_ids, sample(non_cand, size = min(n_non, length(non_cand)), replace = FALSE))
    }
    
    # If we couldn't get enough, fill from remaining pool
    if(length(add_ids) < n_add){
      fill <- sample_from_pool(ind, n_add - length(add_ids), exclude = c(existing, add_ids))
      add_ids <- c(add_ids, fill)
    }
    
    add_ids
  }
  
  # --- persistence parameters ---
  # Pre-policy: moderate stability
  p_keep_pre_base <- 1 - 0.12  # keep ~88% on average
  
  # Post-policy: common shock pushes more change (lower keep),
  # waivers increase stability, denials decrease stability, spillovers decrease stability.
  p_keep_post_base  <- 0.75
  p_keep_waive_add  <- 0.10   # waiver => keep more
  p_keep_deny_add   <- -0.10  # denial => keep less
  p_keep_spill_add  <- -0.25  # exposure_deny pushes keep down (more churn)
  
  # --- container for edges ---
  edges_list <- vector("list", length(years))
  names(edges_list) <- as.character(years)
  
  # --- initialize year0 supplier sets ---
  current_sets <- setNames(vector("list", length(firm_ids)), firm_ids)
  for(fid in firm_ids){
    ind <- firms[firm_id == fid, industry]
    k   <- firms[firm_id == fid, degree0]
    current_sets[[fid]] <- sample_from_pool(ind, k)
  }
  
  edges_list[[as.character(year0)]] <- rbindlist(lapply(firm_ids, function(fid){
    data.table(firm_id = fid, year = year0, supplier_id = current_sets[[fid]])
  }))
  
  # --- simulate years up to pre_policy_end with explicit keep fraction ---
  for(yy in years[years > year0 & years <= pre_policy_end]){
    for(fid in firm_ids){
      ind <- firms[firm_id == fid, industry]
      k   <- panel[firm_id == fid & year == yy, degree0_t]
      
      prev <- current_sets[[fid]]
      if(k == 0L){
        current_sets[[fid]] <- character(0)
      } else {
        # keep fraction (bounded)
        p_keep <- p_keep_pre_base + rnorm(1, 0, 0.02)
        p_keep <- pmin(pmax(p_keep, 0.05), 0.98)
        
        n_keep <- min(length(prev), as.integer(round(p_keep * k)))
        keep   <- if(n_keep > 0L) sample(prev, n_keep, replace = FALSE) else character(0)
        
        n_new  <- k - length(keep)
        add    <- sample_new_suppliers(ind, n_new, keep, p_china = 0.25)
        
        current_sets[[fid]] <- unique(c(keep, add))
      }
    }
    
    edges_list[[as.character(yy)]] <- rbindlist(lapply(firm_ids, function(fid){
      data.table(firm_id = fid, year = yy, supplier_id = current_sets[[fid]])
    }))
  }
  
  # --- compute pre-policy firm-firm overlap weights W from pre_policy_end ---
  pre_edges <- edges_list[[as.character(pre_policy_end)]]
  pre_sets <- split(pre_edges$supplier_id, pre_edges$firm_id)
  
  W <- matrix(0, nrow = n_firms, ncol = n_firms, dimnames = list(firm_ids, firm_ids))
  for(i in seq_len(n_firms)){
    a <- pre_sets[[firm_ids[i]]]
    for(j in seq_len(n_firms)){
      if(i == j) next
      b <- pre_sets[[firm_ids[j]]]
      W[i,j] <- jaccard_vec(a, b)
    }
  }
  W[W < 0.05] <- 0
  diag(W) <- 0
  
  row_sums <- rowSums(W)
  W_norm <- W
  for(i in seq_len(n_firms)){
    if(row_sums[i] > 0) W_norm[i,] <- W[i,] / row_sums[i]
  }
  
  # --- simulate post-policy years with waiver effects + network spillovers ---
  for(yy in years[years > pre_policy_end]){
    
    deny_vec <- panel[year == yy, deny_any]
    names(deny_vec) <- panel[year == yy, firm_id]
    deny_vec <- deny_vec[firm_ids]
    
    exposure_deny <- as.numeric(W_norm %*% deny_vec)
    names(exposure_deny) <- firm_ids
    
    for(fid in firm_ids){
      ind <- firms[firm_id == fid, industry]
      k   <- panel[firm_id == fid & year == yy, degree0_t]
      prev <- current_sets[[fid]]

      own <- panel[firm_id == fid & year == yy]
      waiver <- own$waiver_any
      denied <- own$deny_any
      
      if(k == 0L){
        current_sets[[fid]] <- character(0)
      } else {
        # keep fraction depends on waiver/denial + spillover, bounded
        p_keep <- p_keep_post_base +
          p_keep_waive_add * waiver +
          p_keep_deny_add  * denied +
          p_keep_spill_add * exposure_deny[fid] +
          rnorm(1, 0, 0.02)
        
        p_keep <- pmin(pmax(p_keep, 0.02), 0.99)
        
        n_keep <- min(length(prev), as.integer(round(p_keep * k)))
        keep   <- if(n_keep > 0L) sample(prev, n_keep, replace = FALSE) else character(0)
        
        n_new  <- k - length(keep)
        
        # China add propensity post-policy depending on waiver/denial (same as your logic)
        p_china_add <- if(waiver == 1) 0.24 else if(denied == 1) 0.10 else 0.18
        add <- sample_new_suppliers(ind, n_new, keep, p_china = p_china_add)
        
        current_sets[[fid]] <- unique(c(keep, add))
      }
    }
    
    edges_list[[as.character(yy)]] <- rbindlist(lapply(firm_ids, function(fid){
      data.table(firm_id = fid, year = yy, supplier_id = current_sets[[fid]])
    }))
  }
  
  edges <- rbindlist(edges_list, use.names = TRUE)
  
  # Also compute a couple of "true" firm-year outcomes for validation later
  edges <- merge(edges, suppliers[, .(supplier_id, country)], by="supplier_id", all.x=TRUE)
  firm_year_geo <- edges[, .(
    degree = .N,
    share_china = mean(country == "China", na.rm = TRUE),
    share_usa   = mean(country == "USA", na.rm = TRUE)
  ), by = .(firm_id, year)]
  
  panel <- merge(panel, firm_year_geo, by = c("firm_id","year"), all.x = TRUE)
  panel[is.na(degree), degree := 0L]

  # Pre-tariff China exposure: share_china in the pre_policy_end year
  china_pre <- panel[year == pre_policy_end, .(firm_id, china_exposure_pre = share_china)]
  panel <- merge(panel, china_pre, by = "firm_id", all.x = TRUE)
  panel[is.na(china_exposure_pre), china_exposure_pre := 0]

  list(
    firms = firms,
    suppliers = suppliers,
    pool = pool,
    requests = requests,
    panel = panel,
    edges = edges,
    W_pre = W,
    W_norm = W_norm,
    pre_policy_end = pre_policy_end
  )
}

sim <- simulate_firm_supplier_panel()

# Save simulated objects
saveRDS(sim$panel,     file = file.path(DIR_DATA, "sim_panel.rds"))
saveRDS(sim$edges,     file = file.path(DIR_DATA, "sim_edges.rds"))
saveRDS(sim$requests,  file = file.path(DIR_DATA, "sim_requests.rds"))
saveRDS(sim$firms,     file = file.path(DIR_DATA, "sim_firms.rds"))
saveRDS(sim$suppliers, file = file.path(DIR_DATA, "sim_suppliers.rds"))
saveRDS(sim$W_norm,    file = file.path(DIR_DATA, "sim_W_norm.rds"))

log_msg("Simulation complete. Wrote: data/sim_*.rds")
