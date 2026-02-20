# ============================================================
# 06_tables_figures.R
# Summary statistics + tables + network/heatmap visualizations
# ============================================================

source("R/00_setup.R")
log_msg("Running 06_tables_figures.R ...")

dt <- readRDS(file.path(DIR_DATA, "panel_metrics.rds"))
edges <- readRDS(file.path(DIR_DATA, "sim_edges.rds"))
W_norm <- readRDS(file.path(DIR_DATA, "sim_W_norm.rds"))
mods_fix <- readRDS(file.path(DIR_DATA, "models_fixest.rds"))

setDT(dt); setDT(edges)

dt1 <- dt[!is.na(churn_t_t1)]

# ------------------------------------------------------------
# 1) Summary statistics (CSV + LaTeX)
# ------------------------------------------------------------
vars <- c(
  "churn_t_t1","jaccard_t_t1","divergence_to_base",
  "n_curr","log_degree",
  "waiver_any","deny_any","grant_share",
  "exposure_deny","exposure_waive",
  "log_assets","leverage","share_china","share_usa"
)

sumtab <- rbindlist(lapply(vars, function(v){
  x <- dt1[[v]]
  data.table(
    variable = v,
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    p25 = quantile(x, 0.25, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    p75 = quantile(x, 0.75, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}), use.names = TRUE)

f_csv <- file.path(DIR_OUT_T, "summary_stats.csv")
f_tex <- file.path(DIR_OUT_T, "summary_stats.tex")
f_md  <- file.path(DIR_OUT_T, "summary_stats.md")

fwrite(sumtab, f_csv)

# LaTeX table
latex <- knitr::kable(sumtab, format = "latex", booktabs = TRUE, digits = 3,
                      caption = "Summary statistics (simulation)")
cat(latex, file = f_tex)

# Markdown (handy for quick viewing)
md <- knitr::kable(sumtab, format = "markdown", digits = 3)
cat(md, file = f_md)

log_msg("Wrote summary stats to: ", f_csv, " and ", f_tex)

# ------------------------------------------------------------
# 2) Trend plot: mean churn by waiver status
# ------------------------------------------------------------
trend <- dt1[, .(
  churn = mean(churn_t_t1, na.rm = TRUE),
  jacc = mean(jaccard_t_t1, na.rm = TRUE),
  degree = mean(n_curr, na.rm = TRUE)
), by = .(year, waiver_any)]

p1 <- ggplot(trend, aes(x = year, y = churn, group = waiver_any, color = factor(waiver_any))) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = NULL, y = "Mean churn (1 - Jaccard)", color = "Waiver any",
       title = "Average supplier churn over time by waiver status") +
  theme_minimal()

ggsave(file.path(DIR_OUT_F, "trend_churn_by_waiver.png"), p1, width = 9, height = 5, dpi = 150)

# ------------------------------------------------------------
# 3) Network visualization (bipartite firm-supplier graph for a subset)
# ------------------------------------------------------------
year_pick <- 2019
firms_pick <- sort(unique(dt1$firm_id))[1:25]

edges_sub <- edges[year == year_pick & firm_id %in% firms_pick]
nodes_f <- data.table(name = unique(edges_sub$firm_id), type = "firm")
nodes_s <- data.table(name = unique(edges_sub$supplier_id), type = "supplier")
nodes <- rbind(nodes_f, nodes_s)

g <- igraph::graph_from_data_frame(
  d = data.frame(edges_sub[, .(from = firm_id, to = supplier_id)]),
  vertices = nodes,
  directed = FALSE
)

V(g)$is_firm <- V(g)$type == "firm"
igraph::V(g)$type <- grepl("^S", igraph::V(g)$name)

p_net <- ggraph(g, layout = "bipartite") +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(aes(shape = type), size = 2) +
  theme_void() +
  labs(title = paste0("Firm–supplier network (subset), year ", year_pick),
       shape = "Node type") + 
  theme(legend.position = "none")

ggsave(file.path(DIR_OUT_F, "bipartite_network_subset.png"), p_net, width = 9, height = 6, dpi = 150)

# ------------------------------------------------------------
# 4) Heatmap of firm-firm overlap weights (W_norm) for a subset
# ------------------------------------------------------------
M <- W_norm[firms_pick, firms_pick]
M_long <- as.data.table(as.table(M))
M_long <- M_long %>% 
  dplyr::rename(
    firm_i = V1,
    firm_j = V2,
    w = N 
  )

p_hm <- ggplot(M_long, aes(x = firm_i, y = firm_j, fill = w)) +
  geom_tile() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Overlap", title = "Pre-policy supplier overlap weights (subset)")

ggsave(file.path(DIR_OUT_F, "overlap_heatmap_subset.png"), p_hm, width = 9, height = 7, dpi = 150)

log_msg("06_tables_figures.R complete.")
