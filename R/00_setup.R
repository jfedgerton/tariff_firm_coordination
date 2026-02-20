# ============================================================
# 00_setup.R
# Tariff Waivers × Supplier Networks — Project Skeleton (R)
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(12345)

# ---- helper: install + load packages ----
use_packages <- function(pkgs){
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if(length(missing) > 0){
    message("Installing missing packages: ", paste(missing, collapse=", "))
    install.packages(missing, dependencies = TRUE)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

use_packages(c(
  "data.table",
  "dplyr",
  "tidyr",
  "purrr",
  "ggplot2",
  "stringr",
  "lubridate",
  "fixest",
  "did",
  "texreg",
  "modelsummary",
  "skimr",
  "knitr",
  "kableExtra",
  "broom",
  "igraph",
  "ggraph",
  "patchwork",
  "Matrix",
  "stringdist",
  "WeightIt",
  "cobalt",
  "sna"
))

# ---- project paths ----
ROOT <- getwd()
DIR_DATA   <- file.path(ROOT, "data")
DIR_OUT_T  <- file.path(ROOT, "output", "tables")
DIR_OUT_F  <- file.path(ROOT, "output", "figures")

dir.create(DIR_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_T, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_F, recursive = TRUE, showWarnings = FALSE)

# ---- simple logger ----
log_msg <- function(...){
  cat(sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), ..., "\n")
}

# ---- Jaccard helper for integer vectors (supplier IDs) ----
jaccard_vec <- function(a, b){
  a <- unique(a); b <- unique(b)
  if(length(a) == 0 && length(b) == 0) return(NA_real_)
  inter <- length(intersect(a, b))
  uni   <- length(union(a, b))
  inter / uni
}

# ---- degree-safe division ----
safe_div <- function(num, den){
  ifelse(den == 0, NA_real_, num / den)
}
