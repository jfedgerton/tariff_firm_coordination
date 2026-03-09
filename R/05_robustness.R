# ============================================================
# 05_robustness.R
# Robustness checks suite (templates + runnable examples)
# ============================================================

source("R/00_setup.R")
log_msg("Running 05_robustness.R ...")

dt <- readRDS(file.path(DIR_DATA, "panel_metrics.rds"))
setDT(dt)

dt1 <- dt[!is.na(churn_t_t1)]
dt1[, ever_waiver := as.integer(ever_grant == 1)]
dt1[, ever_deny2  := as.integer(ever_deny == 1)]

# ------------------------------------------------------------
# 0) Convenience: model runner
# ------------------------------------------------------------
run_es_fixest <- function(data, outcome){
  fml <- as.formula(paste0(outcome, " ~ sunab(g_year0, year) + log_assets + leverage | firm_id + year"))
  feols(fml, data = data, cluster = ~ firm_id)
}

# ------------------------------------------------------------
# 1) Alternative outcomes (Jaccard vs baseline divergence vs add/drop/retention)
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
#   A) waiver_any (cumulative) vs B) grant_share vs C) "clean" always-grant vs always-deny sample
# ------------------------------------------------------------
# A) time-varying binary waiver_any (simple FE)
m_alt_t1 <- feols(
  churn_t_t1 ~ waiver_any + exposure_deny + log_assets + leverage | firm_id + year,
  data = dt1, cluster = ~ firm_id
)

# B) intensity
m_alt_t2 <- feols(
  churn_t_t1 ~ grant_share + exposure_deny + log_assets + leverage | firm_id + year,
  data = dt1, cluster = ~ firm_id
)

# C) clean sample: firms that are only grant OR only deny
clean_firms <- dt1[, .(ever_grant = max(ever_grant), ever_deny = max(ever_deny)), by = firm_id][
  xor(ever_grant == 1, ever_deny == 1), firm_id
]
dt_clean <- dt1[firm_id %in% clean_firms]
# define treated = ever_grant (waiver recipients)
dt_clean[, treated_clean := as.integer(ever_grant == 1)]
m_alt_t3 <- feols(
  churn_t_t1 ~ i(post_tariff, treated_clean, ref = 0) + log_assets + leverage | firm_id + year,
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
# 4) Degree trimming (avoid mechanical Jaccard changes from big supplier sets)
# ------------------------------------------------------------
cut <- quantile(dt1$n_curr, probs = 0.99, na.rm = TRUE)
dt_trim <- dt1[n_curr <= cut]
m_trim <- run_es_fixest(dt_trim, "churn_t_t1")

# ------------------------------------------------------------
# 5) Alternative clustering (firm vs firm+industry)
# ------------------------------------------------------------
m_cluster_firm <- feols(
  churn_t_t1 ~ sunab(g_year0, year) + log_assets + leverage | firm_id + year,
  data = dt1, cluster = ~ firm_id
)

m_cluster_firm_ind <- feols(
  churn_t_t1 ~ sunab(g_year0, year) + log_assets + leverage | firm_id + year,
  data = dt1, cluster = ~ firm_id + industry
)

# ------------------------------------------------------------
# 6) Weighting / balancing on baseline covariates (entropy balancing)
#   - build baseline cross-section at 2017 (pre-policy)
# ------------------------------------------------------------
base_year <- 2017
base <- dt1[year == base_year, .(firm_id, ever_waiver, log_assets, leverage, churn_pre = churn_t_t1, industry)]
base <- base[!is.na(churn_pre)]

wobj <- WeightIt::weightit(
  ever_waiver ~ log_assets + leverage + churn_pre + industry,
  data = as.data.frame(base),
  method = "ebal"
)

base_w <- data.table(firm_id = base$firm_id, w = wobj$weights)

dt_w <- merge(dt1, base_w, by = "firm_id", all.x = TRUE)
dt_w[is.na(w), w := 1]

m_weighted <- feols(
  churn_t_t1 ~ sunab(g_year0, year) + log_assets + leverage | firm_id + year,
  data = dt_w, weights = ~ w, cluster = ~ firm_id
)

extract_post02 <- function(m){
  cf <- coef(m)
  nm <- names(cf)
  
  # keep only sunab terms
  idx <- grep("\\:\\:", nm)
  if(length(idx) == 0) return(NA_real_)
  
  nm2 <- nm[idx]
  cf2 <- cf[idx]
  
  # Try to pull the event time as an integer from the tail of the term label.
  # Works across common fixest formats like:
  #   sunab(g_year0, year)::0
  #   sunab(g_year0, year)::1
  #   sunab(g_year0, year)::2
  # or variants with ":year" etc.
  et <- suppressWarnings(as.integer(sub(".*::(-?\\d+)$", "\\1", nm2)))
  keep <- which(!is.na(et) & et %in% 0:2)
  
  if(length(keep) == 0) return(NA_real_)
  mean(cf2[keep])
}

# ------------------------------------------------------------
# 7) Placebo: shuffle event years among treated firms within industry (template)
#   - This is computationally heavier; increase B for real work
# ------------------------------------------------------------
placebo_event_study <- function(data, B = 50, seed = 1){
  set.seed(seed)
  treated_ids <- unique(data[g_year0 > 0, firm_id])
  out <- numeric(B)
  
  for(b in seq_len(B)){
    d <- copy(data)
    
    for(ind in unique(d$industry)){
      ids <- intersect(treated_ids, d[industry == ind, unique(firm_id)])
      if(length(ids) <= 2) next
      
      # firm-level cohort map
      map <- d[industry == ind & firm_id %in% ids,
               .(g = unique(g_year0)[1]),
               by = firm_id]
      map <- map[g > 0]
      if(nrow(map) <= 2) next
      
      # permute cohort years across firms
      map[, g_perm := sample(g, size = .N, replace = FALSE)]
      
      # assign back to all firm-year rows
      d[map, on = "firm_id", g_year0 := i.g_perm]
    }
    
    m <- feols(churn_t_t1 ~ sunab(g_year0, year) + log_assets + leverage | firm_id + year,
               data = d, cluster = ~ firm_id)
    
    out[b] <- extract_post02(m)
  }
  out
}

# Run placebo (increased iterations for publication)
placebo_draws <- placebo_event_study(dt1, B = 500, seed = 123)
saveRDS(placebo_draws, file = file.path(DIR_DATA, "robust_placebo_draws.rds"))

placebo_hist <- ggplot(data.frame(placebo_draws), aes(x = placebo_draws)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(title =  "Placebo distribution (shuffled event years, B=500)",
       x = "Mean post effect (event time 0..2)",
       y = "Count")
ggsave(file.path(DIR_OUT_F, "placebo_distribution.png"), placebo_hist, width = 9, height = 6, dpi = 150)

# ------------------------------------------------------------
# 8) Leave-one-out cohort sensitivity
#   Drop each treatment cohort one at a time and re-estimate
# ------------------------------------------------------------
cohorts <- sort(unique(dt1[g_year0 > 0, g_year0]))
mods_loo <- list()
for(coh in cohorts){
  dd <- dt1[g_year0 != coh | g_year0 == 0]
  mods_loo[[paste0("drop_", coh)]] <- run_es_fixest(dd, "churn_t_t1")
}
saveRDS(mods_loo, file = file.path(DIR_DATA, "robust_loo_cohort.rds"))

texreg::texreg(
  mods_loo,
  custom.model.names = paste0("Drop ", cohorts),
  file = file.path(DIR_OUT_T, "robust_loo_cohort.tex")
)

# ------------------------------------------------------------
# 9) Pre-trend placebo: fake treatment in pre-period
#   Use only pre-2018 data with a fake treatment date (2016)
# ------------------------------------------------------------
dt_pre <- copy(dt1[year <= 2017])
dt_pre[, g_fake := ifelse(ever_waiver == 1, 2016L, 0L)]
m_pretrend_placebo <- feols(
  churn_t_t1 ~ sunab(g_fake, year) + log_assets + leverage | firm_id + year,
  data = dt_pre, cluster = ~ firm_id
)
saveRDS(m_pretrend_placebo, file = file.path(DIR_DATA, "robust_pretrend_placebo.rds"))

# ------------------------------------------------------------
# 10) Heterogeneous treatment effects
#   Estimate by firm size tercile and industry
# ------------------------------------------------------------
dt1[, size_tercile := cut(log_assets, breaks = quantile(log_assets, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                          labels = c("Small", "Medium", "Large"), include.lowest = TRUE)]

mods_hte_size <- list()
for(sz in c("Small", "Medium", "Large")){
  dd <- dt1[size_tercile == sz]
  if(uniqueN(dd[g_year0 > 0, g_year0]) >= 2){
    mods_hte_size[[sz]] <- run_es_fixest(dd, "churn_t_t1")
  }
}
saveRDS(mods_hte_size, file = file.path(DIR_DATA, "robust_hte_size.rds"))

if(length(mods_hte_size) > 0){
  texreg::texreg(
    mods_hte_size,
    custom.model.names = names(mods_hte_size),
    file = file.path(DIR_OUT_T, "robust_hte_size.tex")
  )
}

# Estimate by pre-tariff China exposure tercile
dt1[, china_tercile := cut(china_exposure_pre,
                           breaks = quantile(china_exposure_pre, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                           labels = c("Low", "Medium", "High"), include.lowest = TRUE)]

mods_hte_china <- list()
for(ch in c("Low", "Medium", "High")){
  dd <- dt1[china_tercile == ch]
  if(uniqueN(dd[g_year0 > 0, g_year0]) >= 2){
    mods_hte_china[[ch]] <- run_es_fixest(dd, "churn_t_t1")
  }
}
saveRDS(mods_hte_china, file = file.path(DIR_DATA, "robust_hte_china.rds"))

if(length(mods_hte_china) > 0){
  texreg::texreg(
    mods_hte_china,
    custom.model.names = paste0("China exp: ", names(mods_hte_china)),
    file = file.path(DIR_OUT_T, "robust_hte_china.tex")
  )
}

# Save robustness models
saveRDS(list(
  m_bal = m_bal,
  m_trim = m_trim,
  m_cluster_firm = m_cluster_firm,
  m_cluster_firm_ind = m_cluster_firm_ind,
  m_weighted = m_weighted,
  m_pretrend_placebo = m_pretrend_placebo
), file = file.path(DIR_DATA, "robust_models_misc.rds"))

texreg::texreg(
  list(m_bal, m_trim, m_cluster_firm, m_cluster_firm_ind, m_weighted),
  custom.model.names = c("Balanced panel", "Trim degree (<=p99)", "Cluster firm", "Cluster firm+industry", "Weighted (ebal)"),
  file = file.path(DIR_OUT_T, "robust_misc.tex")
)

log_msg("05_robustness.R complete.")
