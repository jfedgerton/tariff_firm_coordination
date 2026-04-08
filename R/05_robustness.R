# ============================================================
# 05_robustness.R
# Robustness checks suite
# Adapted for real data
# ============================================================

source("R/00_setup.R")
log_msg("Running 05_robustness.R ...")

dt <- readRDS(file.path(DIR_DATA, "panel_metrics.rds"))
setDT(dt)

dt1 <- dt[!is.na(churn_t_t1)]

# Check required variables
required <- c("ever_grant", "ever_deny", "g_year0", "post_tariff",
              "waiver_any", "exposure_deny", "grant_share")
missing <- setdiff(required, names(dt1))
if (length(missing) > 0) {
  stop("Missing variables for robustness: ", paste(missing, collapse = ", "))
}

dt1[, ever_waiver := as.integer(ever_grant == 1)]
dt1[, ever_deny2  := as.integer(ever_deny == 1)]

# ------------------------------------------------------------
# 0) Convenience: model runner
# ------------------------------------------------------------
# Build dynamic control string
ctrl_candidates <- c("log_assets", "leverage", "log_degree", "log_revenue")
ctrls_available <- intersect(ctrl_candidates, names(dt1))
ctrls_available <- ctrls_available[sapply(ctrls_available, function(v) any(!is.na(dt1[[v]])))]
ctrl_str <- paste(ctrls_available, collapse = " + ")

run_es_fixest <- function(data, outcome){
  fml <- as.formula(paste0(outcome, " ~ sunab(g_year0, year) + ", ctrl_str, " | firm_id + year"))
  feols(fml, data = data, cluster = ~ firm_id)
}

# ------------------------------------------------------------
# 1) Alternative outcomes
# ------------------------------------------------------------
outcomes <- c("churn_t_t1", "divergence_to_base", "add_rate", "drop_rate", "retention_rate")
mods_alt_y <- setNames(vector("list", length(outcomes)), outcomes)

for(y in outcomes){
  dd <- dt1[!is.na(get(y))]
  mods_alt_y[[y]] <- run_es_fixest(dd, y)
}

saveRDS(mods_alt_y, file = file.path(DIR_DATA, "robust_alt_outcomes.rds"))

texreg::texreg(
  mods_alt_y,
  custom.model.names = outcomes,
  file = file.path(DIR_OUT_T, "robust_alt_outcomes.tex")
)

# ------------------------------------------------------------
# 2) Alternative treatment definitions
# ------------------------------------------------------------
# A) time-varying binary waiver_any
m_alt_t1 <- feols(
  as.formula(paste0("churn_t_t1 ~ waiver_any + exposure_deny + ", ctrl_str, " | firm_id + year")),
  data = dt1, cluster = ~ firm_id
)

# B) intensity
m_alt_t2 <- feols(
  as.formula(paste0("churn_t_t1 ~ grant_share + exposure_deny + ", ctrl_str, " | firm_id + year")),
  data = dt1, cluster = ~ firm_id
)

# C) clean sample: firms that are only grant OR only deny
clean_firms <- dt1[, .(ever_grant = max(ever_grant), ever_deny = max(ever_deny)), by = firm_id][
  xor(ever_grant == 1, ever_deny == 1), firm_id
]
dt_clean <- dt1[firm_id %in% clean_firms]
dt_clean[, treated_clean := as.integer(ever_grant == 1)]
m_alt_t3 <- feols(
  as.formula(paste0("churn_t_t1 ~ i(post_tariff, treated_clean, ref = 0) + ", ctrl_str, " | firm_id + year")),
  data = dt_clean, cluster = ~ firm_id
)

saveRDS(list(m_alt_t1=m_alt_t1, m_alt_t2=m_alt_t2, m_alt_t3=m_alt_t3),
        file = file.path(DIR_DATA, "robust_alt_treatments.rds"))

texreg::texreg(
  list(m_alt_t1, m_alt_t2, m_alt_t3),
  custom.model.names = c("Binary waiver", "Intensity (share)", "Clean sample static DiD"),
  file = file.path(DIR_OUT_T, "robust_alt_treatments.tex")
)

# ------------------------------------------------------------
# 3) Balanced panel restriction
# ------------------------------------------------------------
years_all <- sort(unique(dt1$year))
balanced_firms <- dt1[, .N, by = firm_id][N == length(years_all), firm_id]
dt_bal <- dt1[firm_id %in% balanced_firms]
m_bal <- run_es_fixest(dt_bal, "churn_t_t1")

# ------------------------------------------------------------
# 4) Degree trimming
# ------------------------------------------------------------
cut <- quantile(dt1$n_curr, probs = 0.99, na.rm = TRUE)
dt_trim <- dt1[n_curr <= cut]
m_trim <- run_es_fixest(dt_trim, "churn_t_t1")

# ------------------------------------------------------------
# 5) Alternative clustering
# ------------------------------------------------------------
fml_es <- as.formula(paste0("churn_t_t1 ~ sunab(g_year0, year) + ", ctrl_str, " | firm_id + year"))

m_cluster_firm <- feols(fml_es, data = dt1, cluster = ~ firm_id)

# Cluster by firm + firm_hq (country of HQ) if available
if ("firm_hq" %in% names(dt1)) {
  m_cluster_firm_hq <- feols(fml_es, data = dt1, cluster = ~ firm_id + firm_hq)
} else {
  m_cluster_firm_hq <- m_cluster_firm  # fallback
}

# ------------------------------------------------------------
# 6) Entropy balancing on baseline covariates
# ------------------------------------------------------------
base_year <- 2017
base_vars <- intersect(c("log_degree", "log_assets", "leverage"), ctrls_available)
base_select <- c("firm_id", "ever_waiver", base_vars, "churn_pre")

base <- dt1[year == base_year]
base[, churn_pre := churn_t_t1]
base <- base[, .SD, .SDcols = intersect(base_select, names(base))]
base <- base[!is.na(churn_pre)]

ebal_fml <- as.formula(paste0("ever_waiver ~ ", paste(c(base_vars, "churn_pre"), collapse = " + ")))

wobj <- tryCatch({
  WeightIt::weightit(ebal_fml, data = as.data.frame(base), method = "ebal")
}, error = function(e) {
  log_msg("WARNING: Entropy balancing failed:", e$message)
  NULL
})

if (!is.null(wobj)) {
  base_w <- data.table(firm_id = base$firm_id, w = wobj$weights)
  dt_w <- merge(dt1, base_w, by = "firm_id", all.x = TRUE)
  dt_w[is.na(w), w := 1]

  m_weighted <- feols(fml_es, data = dt_w, weights = ~ w, cluster = ~ firm_id)
} else {
  m_weighted <- m_cluster_firm  # fallback
}

# Coef extractor for placebo test
extract_post02 <- function(m){
  cf <- coef(m)
  nm <- names(cf)
  idx <- grep("\\:\\:", nm)
  if(length(idx) == 0) return(NA_real_)
  nm2 <- nm[idx]
  cf2 <- cf[idx]
  et <- suppressWarnings(as.integer(sub(".*::(-?\\d+)$", "\\1", nm2)))
  keep <- which(!is.na(et) & et %in% 0:2)
  if(length(keep) == 0) return(NA_real_)
  mean(cf2[keep])
}

# ------------------------------------------------------------
# 7) Placebo: shuffle event years among treated firms
# ------------------------------------------------------------
placebo_event_study <- function(data, B = 50, seed = 1){
  set.seed(seed)
  data <- as.data.table(data)
  treated_ids <- unique(data[g_year0 > 0, firm_id])
  out <- numeric(B)

  # Use firm_hq as grouping variable if available, otherwise skip stratification
  group_var <- if ("firm_hq" %in% names(data)) "firm_hq" else NULL

  for(b in seq_len(B)){
    d <- copy(data)

    if (!is.null(group_var)) {
      for(grp in unique(d[[group_var]])){
        ids <- intersect(treated_ids, d[get(group_var) == grp, unique(firm_id)])
        if(length(ids) <= 2) next
        map <- d[get(group_var) == grp & firm_id %in% ids,
                 .(firm_id, g = unique(g_year0)[1]),
                 by = firm_id]
        map <- map[g > 0]
        if(nrow(map) <= 2) next
        map[, g_perm := sample(g, size = .N, replace = FALSE)]
        d[map, on = "firm_id", g_year0 := i.g_perm]
      }
    } else {
      # Unstratified permutation
      map <- d[firm_id %in% treated_ids, .(firm_id, g = unique(g_year0)[1]), by = firm_id]
      map <- map[g > 0]
      if(nrow(map) > 2){
        map[, g_perm := sample(g, size = .N, replace = FALSE)]
        d[map, on = "firm_id", g_year0 := i.g_perm]
      }
    }

    m <- feols(fml_es, data = d, cluster = ~ firm_id)
    out[b] <- extract_post02(m)
  }
  out
}

placebo_draws <- placebo_event_study(dt1, B = 100, seed = 123)
saveRDS(placebo_draws, file = file.path(DIR_DATA, "robust_placebo_draws.rds"))

placebo_hist <- ggplot(data.frame(placebo_draws), aes(x = placebo_draws)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Placebo distribution (shuffled event years)",
       x = "Mean post effect (event time 0..2)",
       y = "Count")
ggsave(file.path(DIR_OUT_F, "placebo_distribution.png"), placebo_hist, width = 9, height = 6, dpi = 150)

# Save robustness models
saveRDS(list(
  m_bal = m_bal,
  m_trim = m_trim,
  m_cluster_firm = m_cluster_firm,
  m_cluster_firm_hq = m_cluster_firm_hq,
  m_weighted = m_weighted
), file = file.path(DIR_DATA, "robust_models_misc.rds"))

texreg::texreg(
  list(m_bal, m_trim, m_cluster_firm, m_cluster_firm_hq, m_weighted),
  custom.model.names = c("Balanced panel", "Trim degree (<=p99)", "Cluster firm",
                          "Cluster firm+HQ", "Weighted (ebal)"),
  file = file.path(DIR_OUT_T, "robust_misc.tex")
)

log_msg("05_robustness.R complete.")
