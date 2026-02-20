# ============================================================
# 03_models.R
# Main model specifications (TWFE, event-study, C&S DID, DDD)
# ============================================================

source("R/00_setup.R")
log_msg("Running 03_models.R ...")

dt <- readRDS(file.path(DIR_DATA, "panel_metrics.rds"))
setDT(dt)

# Keep years where we can compute t vs t-1 (drop first year)
dt1 <- dt[!is.na(jaccard_t_t1)]

# -------------------------
# Model specs (fixest)
# -------------------------
# Baseline controls (editable)
ctrls <- ~ log_assets + leverage + log_degree

# (1) Simple TWFE with time-varying waiver status (NOT a staggered estimator, but a baseline)
m_twfe_1 <- feols(
  churn_t_t1 ~ waiver_any + deny_any + post_tariff + exposure_deny + exposure_waive + log_assets + leverage + log_degree |
    firm_id + year,
  data = dt1,
  cluster = ~ firm_id
)

# (2) "Classic" DiD style (static ever_grant group × post_tariff) — mirrors early proposal structure
# WARNING: for staggered adoption this can be biased; keep as a diagnostic.
dt1[, ever_waiver := as.integer(ever_grant == 1)]

m_did_static <- feols(
  churn_t_t1 ~ i(post_tariff, ever_waiver, ref = 0) + log_assets + leverage | firm_id + year,
  data = dt1,
  cluster = ~ firm_id
)

# (3) Event-study (staggered adoption) using sunab: first waiver grant year g_year0
# g_year0 == 0 means never-treated
m_es <- feols(
  churn_t_t1 ~ sunab(g_year0, year) + log_assets + leverage | firm_id + year,
  data = dt1,
  cluster = ~ firm_id
)

# (4) DDD-style event-study: allow differential effects by political sensitivity
m_es_ddd <- feols(
  churn_t_t1 ~ sunab(g_year0, year) * log_assets + leverage | firm_id + year,
  data = dt1,
  cluster = ~ firm_id
)

# (5) "Dose response" style (continuous waiver intensity) — simple FE (not a full dynamic dose response)
m_intensity <- feols(
  churn_t_t1 ~ grant_share + exposure_deny + log_assets + leverage | firm_id + year,
  data = dt1,
  cluster = ~ firm_id
)

# Save model objects
saveRDS(list(
  m_twfe_1 = m_twfe_1,
  m_did_static = m_did_static,
  m_es = m_es,
  m_es_ddd = m_es_ddd,
  m_intensity = m_intensity
), file = file.path(DIR_DATA, "models_fixest.rds"))

# Export texreg table
texreg::texreg(
  list(m_twfe_1, m_did_static, m_es, m_es_ddd, m_intensity),
  digits = 3,
  stars = c(0.001, 0.01, 0.05, 0.1),
  custom.model.names = c("TWFE + spillovers", "Static DiD", "Event-study", "Event-study × sensitivity", "Intensity FE"),
  file = file.path(DIR_OUT_T, "reg_main_fixest.tex")
)
log_msg("Wrote: output/tables/reg_main_fixest.tex")

# Event-study plot (fixest)
png(file.path(DIR_OUT_F, "event_study_fixest.png"), width = 1000, height = 700)
iplot(m_es, main = "Event-study: effect of first waiver grant on churn (1-Jaccard)",
      xlab = "Event time (years relative to first grant)",
      ylab = "Effect on churn", ref.line = 0)
dev.off()

# -------------------------
# Callaway–Sant'Anna DID (did package)
# -------------------------
# did::att_gt requires:
# - yname, tname, idname, gname (0 for never-treated)
# - panel data, can include covariates
dt1[, g_year0 := as.numeric(g_year0)]   # ensure double
dt1[is.na(g_year0), g_year0 := 0] 
dt1 <- as.data.frame(dt1)
dt1$firm_id_num <- as.numeric(factor(dt1$firm_id))
cs <- att_gt(
  yname  = "churn_t_t1",
  tname  = "year",
  idname = "firm_id_num",
  gname  = "g_year0",
  xformla = ~ log_assets + leverage + log_degree,
  data = dt1,
  control_group = "notyettreated",
  est_method = "dr"
)

cs_ag <- aggte(cs, type = "dynamic")  

saveRDS(list(cs = cs, cs_ag = cs_ag), file = file.path(DIR_DATA, "models_did_cs.rds"))

# Plot C&S event-time
egt <- cs_ag$egt
att <- if (!is.null(cs_ag$att.egt)) cs_ag$att.egt else cs_ag$att
se  <- if (!is.null(cs_ag$se.egt))  cs_ag$se.egt  else cs_ag$se

ub <- att + 1.96 * se
lb <- att - 1.96 * se

png(file.path(DIR_OUT_F, "event_study_callaway_santanna.png"), width = 1000, height = 700)
plot(egt, att, type = "b", xlab = "Event time", ylab = "ATT (churn)", main = "")
lines(egt, ub, lty = 2)
lines(egt, lb, lty = 2)
abline(h = 0, lty = 3)
title("Callaway–Sant'Anna dynamic ATT: churn (1-Jaccard)")
dev.off()

log_msg("03_models.R complete.")
