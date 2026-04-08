# ============================================================
# 03_models.R
# Main model specifications (TWFE, event-study, C&S DID, DDD)
# Adapted for real data
# ============================================================

source("R/00_setup.R")
log_msg("Running 03_models.R ...")

dt <- readRDS(file.path(DIR_DATA, "panel_metrics.rds"))
setDT(dt)

# Keep years where we can compute t vs t-1 (drop first year)
dt1 <- dt[!is.na(jaccard_t_t1)]

# ---- Check that exemption variables exist ----
required_vars <- c("waiver_any", "deny_any", "exposure_deny", "exposure_waive",
                    "grant_share", "g_year0", "ever_grant")
missing_vars <- setdiff(required_vars, names(dt1))

if (length(missing_vars) > 0) {
  log_msg("ERROR: Missing required variables:", paste(missing_vars, collapse = ", "))
  log_msg("       Load exemption data in 01_load_data.R and rebuild metrics.")
  stop("Missing exemption variables. Cannot run models.")
}

# -------------------------
# Model specs (fixest)
# -------------------------
# Build control formula dynamically based on what's available
ctrl_candidates <- c("log_assets", "leverage", "log_degree", "log_revenue")
ctrls_available <- intersect(ctrl_candidates, names(dt1))
# Drop controls that are all-NA
ctrls_available <- ctrls_available[sapply(ctrls_available, function(v) any(!is.na(dt1[[v]])))]
ctrl_str <- paste(ctrls_available, collapse = " + ")
log_msg("Available controls:", ctrl_str)

# (1) TWFE with time-varying waiver status + spillovers
fml1 <- as.formula(paste0("churn_t_t1 ~ waiver_any + deny_any + post_tariff + ",
                           "exposure_deny + exposure_waive + ", ctrl_str, " | firm_id + year"))
m_twfe_1 <- feols(fml1, data = dt1, cluster = ~ firm_id)

# (2) Static DiD: ever_grant × post_tariff
dt1[, ever_waiver := as.integer(ever_grant == 1)]

fml2 <- as.formula(paste0("churn_t_t1 ~ i(post_tariff, ever_waiver, ref = 0) + ",
                           ctrl_str, " | firm_id + year"))
m_did_static <- feols(fml2, data = dt1, cluster = ~ firm_id)

# (3) Event-study (staggered adoption) using sunab
fml3 <- as.formula(paste0("churn_t_t1 ~ sunab(g_year0, year) + ", ctrl_str, " | firm_id + year"))
m_es <- feols(fml3, data = dt1, cluster = ~ firm_id)

# (4) DDD-style: differential effects by industry or sensitivity
# Using industry (SIC 2-digit) if available, otherwise log_degree
het_var <- if ("industry" %in% names(dt1) && any(!is.na(dt1$industry))) "industry" else "log_degree"
fml4 <- as.formula(paste0("churn_t_t1 ~ sunab(g_year0, year) * ", het_var, " + ",
                           ctrl_str, " | firm_id + year"))
m_es_ddd <- feols(fml4, data = dt1, cluster = ~ firm_id)

# (5) Dose response (continuous waiver intensity)
fml5 <- as.formula(paste0("churn_t_t1 ~ grant_share + exposure_deny + ",
                           ctrl_str, " | firm_id + year"))
m_intensity <- feols(fml5, data = dt1, cluster = ~ firm_id)

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
  custom.model.names = c("TWFE + spillovers", "Static DiD", "Event-study",
                          "Event-study x heterogeneity", "Intensity FE"),
  file = file.path(DIR_OUT_T, "reg_main_fixest.tex")
)
log_msg("Wrote: output/tables/reg_main_fixest.tex")

# Event-study plot
png(file.path(DIR_OUT_F, "event_study_fixest.png"), width = 1000, height = 700)
iplot(m_es, main = "Event-study: effect of first exemption grant on churn (1-Jaccard)",
      xlab = "Event time (years relative to first grant)",
      ylab = "Effect on churn", ref.line = 0)
dev.off()

# -------------------------
# Callaway-Sant'Anna DID
# -------------------------
dt1[, g_year0 := as.numeric(g_year0)]
dt1[is.na(g_year0), g_year0 := 0]
dt1 <- as.data.frame(dt1)
dt1$firm_id_num <- as.numeric(factor(dt1$firm_id))

# Build xformla from available controls
cs_xformla <- as.formula(paste0("~ ", ctrl_str))

cs <- att_gt(
  yname  = "churn_t_t1",
  tname  = "year",
  idname = "firm_id_num",
  gname  = "g_year0",
  xformla = cs_xformla,
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
title("Callaway-Sant'Anna dynamic ATT: churn (1-Jaccard)")
dev.off()

log_msg("03_models.R complete.")
