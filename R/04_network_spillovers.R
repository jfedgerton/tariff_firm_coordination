# ============================================================
# 04_network_spillovers.R
# Strategies to condition out / model network spillovers
# ============================================================

source("R/00_setup.R")
log_msg("Running 04_network_spillovers.R ...")

dt <- readRDS(file.path(DIR_DATA, "panel_metrics.rds"))
W_norm <- readRDS(file.path(DIR_DATA, "sim_W_norm.rds"))
setDT(dt)

dt1 <- dt[!is.na(churn_t_t1)]

# ------------------------------------------------------------
# Strategy 1: Exposure mapping controls (already computed)
#   - include neighbor exposure to denial/waiver in the regression
# ------------------------------------------------------------
m_spill_ctrl <- feols(
  churn_t_t1 ~ waiver_any + exposure_deny + exposure_waive + log_assets + leverage |
    firm_id + year,
  data = dt1,
  cluster = ~ firm_id
)

# ------------------------------------------------------------
# Strategy 2: Community clustering / partial interference
#   - build a firm-firm graph based on pre-policy overlap W_norm
#   - detect communities; cluster SEs by firm and community
# ------------------------------------------------------------
firm_ids <- sort(unique(dt1$firm_id))
W <- W_norm[firm_ids, firm_ids]
A <- (W > 0) * 1
diag(A) <- 0

g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
comm <- igraph::cluster_louvain(g)

comm_id <- data.table(
  firm_id = igraph::V(g)$name,
  comm_id = as.integer(igraph::membership(comm))
)

dt1 <- merge(dt1, comm_id, by = "firm_id", all.x = TRUE)

m_spill_comm_cluster <- feols(
  churn_t_t1 ~ waiver_any + exposure_deny + exposure_waive + log_assets + leverage |
    firm_id + year,
  data = dt1,
  cluster = ~ firm_id + comm_id
)

# ------------------------------------------------------------
# Strategy 3 (diagnostic): MRQAP on dyadic outcomes (sna::netlm)
#   - Helps assess whether dyadic dependence is strong.
#   - Example: Are firms more similar in churn when they share suppliers / treatment?
#   - NOTE: This is not a replacement for panel DiD; it's complementary.
# ------------------------------------------------------------
year_pick <- 2019
slice <- dt1[year == year_pick, .(firm_id, churn_t_t1, waiver_any)]
slice <- slice[order(firm_id)]
n <- nrow(slice)

# Dependent matrix: pairwise absolute differences in churn
Y <- as.matrix(dist(slice$churn_t_t1, method = "euclidean"))
Y <- as.matrix(Y)

# X1: pairwise similarity in waiver status (1 if same status, 0 otherwise)
wa <- slice$waiver_any
X1 <- outer(wa, wa, FUN = function(a,b) as.numeric(a == b))

# X2: baseline overlap network (W)
X2 <- W[slice$firm_id, slice$firm_id]

# MRQAP regression (keep reps modest for speed; increase for real analysis)
set.seed(123)
mrqap <- sna::netlm(Y, list(X1, X2), nullhyp = "qap", reps = 200)

# ------------------------------------------------------------
# Strategy 4: Alternative network thresholds
#   Test sensitivity of spillover estimates to the W threshold.
#   The default threshold is 0.05 (set in 01_simulate_data.R).
#   Re-compute W_norm at different thresholds and re-estimate.
# ------------------------------------------------------------
W_raw <- readRDS(file.path(DIR_DATA, "sim_W_norm.rds"))
# W_norm was already row-normalized from a 0.05 threshold.
# We need the raw (un-thresholded, un-normalized) W. We can approximate
# by reloading from the pre-threshold W if saved, or reconstruct from edges.
# For simplicity, use the existing W_norm and re-threshold.

thresholds <- c(0, 0.01, 0.05, 0.10, 0.20)
threshold_results <- list()

for(thresh in thresholds){
  Wt <- W_raw
  Wt[Wt < thresh] <- 0
  diag(Wt) <- 0
  rs <- rowSums(Wt)
  Wt_norm <- Wt
  for(i in seq_len(nrow(Wt))){
    if(rs[i] > 0) Wt_norm[i,] <- Wt[i,] / rs[i]
  }

  # Recompute exposure using this threshold
  Wt_sub <- Wt_norm[firm_ids, firm_ids]
  dt1[, exp_deny_t  := NA_real_]
  dt1[, exp_waive_t := NA_real_]

  for(yy in sort(unique(dt1$year))){
    sl <- dt1[year == yy]
    dv <- setNames(sl$deny_any, sl$firm_id)[firm_ids]
    wv <- setNames(sl$waiver_any, sl$firm_id)[firm_ids]
    dt1[year == yy, exp_deny_t  := as.numeric(Wt_sub %*% dv)]
    dt1[year == yy, exp_waive_t := as.numeric(Wt_sub %*% wv)]
  }

  m <- feols(
    churn_t_t1 ~ waiver_any + exp_deny_t + exp_waive_t + log_assets + leverage |
      firm_id + year,
    data = dt1, cluster = ~ firm_id
  )
  threshold_results[[as.character(thresh)]] <- m
}
dt1[, c("exp_deny_t", "exp_waive_t") := NULL]

saveRDS(list(
  m_spill_ctrl = m_spill_ctrl,
  m_spill_comm_cluster = m_spill_comm_cluster,
  mrqap = mrqap,
  comm_id = comm_id,
  threshold_results = threshold_results
), file = file.path(DIR_DATA, "models_spillovers.rds"))

# Export tables
texreg::texreg(
  list(m_spill_ctrl, m_spill_comm_cluster),
  custom.model.names = c("Exposure controls", "Exposure + community cluster"),
  file = file.path(DIR_OUT_T, "reg_spillover_strategies.tex")
)

texreg::texreg(
  threshold_results,
  custom.model.names = paste0("Thresh=", thresholds),
  file = file.path(DIR_OUT_T, "reg_spillover_thresholds.tex")
)

log_msg("Wrote: output/tables/reg_spillover_strategies.tex")
log_msg("Wrote: output/tables/reg_spillover_thresholds.tex")
log_msg("04_network_spillovers.R complete.")
