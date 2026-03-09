# ============================================================
# 07_simulation_validation.R
# Simulation validation checks: null DGP, dose-response, power
# ============================================================

source("R/00_setup.R")
log_msg("Running 07_simulation_validation.R ...")

# We re-use the simulation function from 01 but with modified parameters.
# Source it to get simulate_firm_supplier_panel()
# (The function is defined inside 01_simulate_data.R; we extract it here.)

# ---- Helper: run one simulation + estimation cycle ----
run_sim_and_estimate <- function(
  p_keep_waive_add = 0.10,
  p_keep_deny_add  = -0.10,
  n_firms = 300,
  seed = NULL
){
  if(!is.null(seed)) set.seed(seed)

  # Inline a minimal simulation + estimation pipeline
  # (avoids sourcing 01/02/03 which write files)
  source("R/00_setup.R")

  # Temporarily override parameters by re-running simulation with modified values
  # We use a lightweight approach: modify the simulation function's environment
  sim_env <- new.env(parent = globalenv())

  # Source the simulation function
  sim_code <- readLines("R/01_simulate_data.R")
  # Extract only the function definition (lines 9-350 approximately)
  func_start <- grep("^simulate_firm_supplier_panel", sim_code)[1]
  func_end <- grep("^sim <-", sim_code)[1] - 1

  eval(parse(text = sim_code[func_start:func_end]), envir = sim_env)

  # Override the persistence parameters inside the function
  # We do this by creating a wrapper
  original_fn <- sim_env$simulate_firm_supplier_panel

  # Run simulation with specified parameters
  # The original function has hardcoded persistence params inside.
  # For clean parameterization, we modify the source text directly.
  modified_code <- sim_code[func_start:func_end]
  modified_code <- gsub(
    "p_keep_waive_add  <- 0\\.10",
    sprintf("p_keep_waive_add  <- %.4f", p_keep_waive_add),
    modified_code
  )
  modified_code <- gsub(
    "p_keep_deny_add   <- -0\\.10",
    sprintf("p_keep_deny_add   <- %.4f", p_keep_deny_add),
    modified_code
  )
  if(n_firms != 300){
    modified_code <- gsub(
      "n_firms = 300",
      sprintf("n_firms = %d", n_firms),
      modified_code
    )
  }

  eval(parse(text = modified_code), envir = sim_env)

  sim <- sim_env$simulate_firm_supplier_panel(seed = ifelse(is.null(seed), 123, seed))

  panel <- sim$panel
  edges <- sim$edges
  W_norm <- sim$W_norm
  setDT(panel); setDT(edges)

  # Build metrics (minimal version of 02_build_metrics.R)
  sets <- edges[, .(suppliers = list(sort(unique(supplier_id)))), by = .(firm_id, year)]
  panel <- merge(panel, sets, by = c("firm_id","year"), all.x = TRUE)

  is_missing_list <- function(x) is.null(x) || (length(x) == 1L && is.na(x[1]))
  miss_idx <- which(vapply(panel$suppliers, is_missing_list, logical(1)))
  if(length(miss_idx) > 0L) panel[miss_idx, suppliers := replicate(.N, list(character(0)), simplify = FALSE)]

  setorder(panel, firm_id, year)

  lag_map <- panel[, .(firm_id, year = year + 1L, suppliers_lag = suppliers)]
  panel <- merge(panel, lag_map, by = c("firm_id", "year"), all.x = TRUE, sort = FALSE)

  miss_lag <- which(vapply(panel$suppliers_lag, is_missing_list, logical(1)))
  panel[, has_genuine_lag := TRUE]
  if(length(miss_lag) > 0L){
    panel[miss_lag, has_genuine_lag := FALSE]
    panel[miss_lag, suppliers_lag := replicate(.N, list(character(0)), simplify = FALSE)]
  }

  panel[, jaccard_t_t1 := mapply(jaccard_vec, suppliers, suppliers_lag) |> as.numeric()]
  panel[, churn_t_t1 := 1 - jaccard_t_t1]
  panel[has_genuine_lag == FALSE, c("jaccard_t_t1", "churn_t_t1") := NA_real_]

  panel[, log_degree := log1p(lengths(suppliers))]
  panel[, post_tariff := as.integer(year >= 2018)]
  panel[, g_year0 := ifelse(is.na(g_year), 0L, g_year)]

  dt1 <- panel[!is.na(churn_t_t1)]

  # Estimate event study
  m <- feols(
    churn_t_t1 ~ sunab(g_year0, year) + log_assets + leverage | firm_id + year,
    data = dt1, cluster = ~ firm_id
  )

  # Extract mean post-treatment effect (event times 0-2)
  cf <- coef(m)
  nm <- names(cf)
  idx <- grep("\\:\\:", nm)
  if(length(idx) == 0) return(list(att = NA_real_, pval = NA_real_))

  et <- suppressWarnings(as.integer(sub(".*::(-?\\d+)$", "\\1", nm[idx])))
  keep <- which(!is.na(et) & et %in% 0:2)
  if(length(keep) == 0) return(list(att = NA_real_, pval = NA_real_))

  att <- mean(cf[idx[keep]])

  # Get p-value from the sunab aggregated test
  se_vec <- sqrt(diag(vcov(m)))[idx[keep]]
  t_stat <- mean(cf[idx[keep]] / se_vec)
  pval <- 2 * pnorm(-abs(t_stat))

  list(att = att, pval = pval)
}


# ============================================================
# CHECK 1: Null DGP — no treatment effect
# ============================================================
log_msg("Check 1: Null DGP (p_keep_waive_add = 0, p_keep_deny_add = 0)")

null_results <- list()
for(s in 1:10){
  res <- tryCatch(
    run_sim_and_estimate(p_keep_waive_add = 0, p_keep_deny_add = 0, seed = s * 100),
    error = function(e) list(att = NA_real_, pval = NA_real_)
  )
  null_results[[s]] <- res
  cat(sprintf("  Seed %d: ATT = %.4f, p = %.4f\n", s * 100, res$att, res$pval))
}

null_atts <- sapply(null_results, `[[`, "att")
null_pvals <- sapply(null_results, `[[`, "pval")
cat(sprintf("  Mean ATT under null: %.4f (should be ~0)\n", mean(null_atts, na.rm = TRUE)))
cat(sprintf("  Rejection rate (p<0.05): %.2f (should be ~0.05)\n", mean(null_pvals < 0.05, na.rm = TRUE)))


# ============================================================
# CHECK 2: Dose-response monotonicity
# ============================================================
log_msg("Check 2: Dose-response (varying p_keep_waive_add from 0 to 0.20)")

doses <- c(0, 0.05, 0.10, 0.15, 0.20)
dose_results <- numeric(length(doses))

for(i in seq_along(doses)){
  res <- tryCatch(
    run_sim_and_estimate(p_keep_waive_add = doses[i], seed = 42),
    error = function(e) list(att = NA_real_)
  )
  dose_results[i] <- res$att
  cat(sprintf("  dose = %.2f: ATT = %.4f\n", doses[i], res$att))
}

is_monotone <- all(diff(dose_results) <= 0, na.rm = TRUE)
cat(sprintf("  Monotonically decreasing ATTs: %s\n",
            ifelse(is_monotone, "YES (waivers reduce churn)", "NO — check specification")))

# Save dose-response plot
dose_df <- data.frame(dose = doses, att = dose_results)
p_dose <- ggplot(dose_df, aes(x = dose, y = att)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 0, lty = 2) +
  theme_minimal() +
  labs(x = "Waiver persistence parameter (p_keep_waive_add)",
       y = "Estimated ATT on churn",
       title = "Dose-response: treatment intensity vs estimated effect")
ggsave(file.path(DIR_OUT_F, "validation_dose_response.png"), p_dose, width = 9, height = 6, dpi = 150)


# ============================================================
# CHECK 3: Power analysis (varying sample size)
# ============================================================
log_msg("Check 3: Power analysis (varying n_firms)")

sample_sizes <- c(100, 200, 300, 500)
power_results <- data.table(n_firms = integer(), att = numeric(), pval = numeric(), seed = integer())

for(n in sample_sizes){
  for(s in 1:5){
    res <- tryCatch(
      run_sim_and_estimate(n_firms = n, seed = s * 10 + n),
      error = function(e) list(att = NA_real_, pval = NA_real_)
    )
    power_results <- rbind(power_results, data.table(n_firms = n, att = res$att, pval = res$pval, seed = s))
  }
}

power_summary <- power_results[, .(
  mean_att = mean(att, na.rm = TRUE),
  reject_rate = mean(pval < 0.05, na.rm = TRUE)
), by = n_firms]

cat("\n  Power analysis results:\n")
print(power_summary)

# Save power plot
p_power <- ggplot(power_summary, aes(x = n_firms, y = reject_rate)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 0.80, lty = 2, color = "red") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  labs(x = "Number of firms", y = "Rejection rate (p < 0.05)",
       title = "Power analysis: sample size vs detection probability")
ggsave(file.path(DIR_OUT_F, "validation_power_analysis.png"), p_power, width = 9, height = 6, dpi = 150)


# ============================================================
# Save all validation results
# ============================================================
saveRDS(list(
  null_results = null_results,
  dose_results = data.frame(dose = doses, att = dose_results),
  power_results = power_results,
  power_summary = power_summary
), file = file.path(DIR_DATA, "validation_results.rds"))

log_msg("07_simulation_validation.R complete.")
