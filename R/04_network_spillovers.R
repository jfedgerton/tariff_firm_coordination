# ============================================================
# 04_network_spillovers.R
# Strategies to condition out / model network spillovers
# Adapted for real data
# ============================================================

source("R/00_setup.R")
log_msg("Running 04_network_spillovers.R ...")

dt     <- readRDS(file.path(DIR_DATA, "panel_metrics.rds"))
W_norm <- readRDS(file.path(DIR_DATA, "W_norm.rds"))
setDT(dt)

dt1 <- dt[!is.na(churn_t_t1)]

# Check required variables
if (!all(c("waiver_any", "exposure_deny", "exposure_waive") %in% names(dt1))) {
  stop("Missing waiver/exposure variables. Load exemption data first.")
}

# ------------------------------------------------------------
# Strategy 1: Exposure mapping controls (already computed in 02)
# ------------------------------------------------------------
m_spill_ctrl <- feols(
  churn_t_t1 ~ waiver_any + exposure_deny + exposure_waive + log_degree |
    firm_id + year,
  data = dt1,
  cluster = ~ firm_id
)

# ------------------------------------------------------------
# Strategy 2: Community clustering / partial interference
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
  churn_t_t1 ~ waiver_any + exposure_deny + exposure_waive + log_degree |
    firm_id + year,
  data = dt1,
  cluster = ~ firm_id + comm_id
)

# ------------------------------------------------------------
# Strategy 3 (diagnostic): MRQAP on dyadic outcomes
# ------------------------------------------------------------
# Pick a post-policy year for the cross-section
year_pick <- 2019
slice <- dt1[year == year_pick, .(firm_id, churn_t_t1, waiver_any)]
slice <- slice[order(firm_id)]

# Subset to firms in W_norm
slice <- slice[firm_id %in% rownames(W_norm)]
n <- nrow(slice)

if (n > 5) {
  Y <- as.matrix(dist(slice$churn_t_t1, method = "euclidean"))

  wa <- slice$waiver_any
  X1 <- outer(wa, wa, FUN = function(a,b) as.numeric(a == b))
  X2 <- W_norm[slice$firm_id, slice$firm_id]

  set.seed(123)
  mrqap <- sna::netlm(Y, list(X1, X2), nullhyp = "qap", reps = 200)
} else {
  log_msg("WARNING: Too few firms for MRQAP in year", year_pick)
  mrqap <- NULL
}

saveRDS(list(
  m_spill_ctrl = m_spill_ctrl,
  m_spill_comm_cluster = m_spill_comm_cluster,
  mrqap = mrqap,
  comm_id = comm_id
), file = file.path(DIR_DATA, "models_spillovers.rds"))

texreg::texreg(
  list(m_spill_ctrl, m_spill_comm_cluster),
  custom.model.names = c("Exposure controls", "Exposure + community cluster"),
  file = file.path(DIR_OUT_T, "reg_spillover_strategies.tex")
)

log_msg("Wrote: output/tables/reg_spillover_strategies.tex")
log_msg("04_network_spillovers.R complete.")
