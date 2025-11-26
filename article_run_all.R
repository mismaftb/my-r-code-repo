# code/article_run_all.R — Driver: run all models, LOYO, tables, figs, diagnostics
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2)
})
source("code/utils.R")
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

dir.create("outputs/article/models",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/article/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/article/figs",    recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/article/metrics", recursive = TRUE, showWarnings = FALSE)

safe_source <- function(f) {
  if (file.exists(f)) {
    message("Running: ", f)
    try(source(f, echo = FALSE, max.deparse.length = 200), silent = FALSE)
  } else {
    warning("Missing file: ", f)
  }
}

message("=== MODELS ===")
safe_source("code/in_ok.R")
safe_source("code/in_nk.R")
safe_source("code/un_ok.R")
safe_source("code/un_nk.R")

message("=== LOYO ===")
safe_source("code/loyo_in_ok.R")
safe_source("code/loyo_in_nk.R")
safe_source("code/loyo_un_ok.R")
safe_source("code/loyo_un_nk.R")

message("=== TABLES 1–4 ===")
safe_source("code/make_table1_in_ok.R")
safe_source("code/make_table2_in_nk.R")
safe_source("code/make_table3_un_ok.R")
safe_source("code/make_table4_un_nk.R")

message("=== TABLE 5 (LOYO RMSE) ===")
paths <- list(
  IN_OK = "outputs/article/metrics/IN_OK_LOYO_RMSE.csv",
  IN_NK = "outputs/article/metrics/IN_NK_LOYO_RMSE.csv",
  UN_OK = "outputs/article/metrics/UN_OK_LOYO_RMSE.csv",
  UN_NK = "outputs/article/metrics/UN_NK_LOYO_RMSE.csv"
)
read_rmse <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  df <- readr::read_csv(path, show_col_types = FALSE)
  mean(df$RMSE, na.rm = TRUE)
}
tbl5 <- data.frame(
  Category = c("Inflation","Inflation","Unemployment","Unemployment"),
  Spec     = c("OK","NK","OK","NK"),
  Model    = c("IN_OK","IN_NK","UN_OK","UN_NK"),
  Mean_RMSE = c(
    read_rmse(paths$IN_OK),
    read_rmse(paths$IN_NK),
    read_rmse(paths$UN_OK),
    read_rmse(paths$UN_NK)
  ),
  stringsAsFactors = FALSE
)
tbl5$Winner <- ave(tbl5$Mean_RMSE, tbl5$Category, FUN = function(x) x == min(x, na.rm = TRUE))
print(tbl5)
readr::write_excel_csv(tbl5, "outputs/article/tables/Table5_LOYO_RMSE.csv")

message("=== DM TESTS ===")
# Inflation DM
if (file.exists(paths$IN_OK) && file.exists(paths$IN_NK)) {
  ok <- readr::read_csv(paths$IN_OK, show_col_types = FALSE) %>% dplyr::rename(RMSE_OK = RMSE)
  nk <- readr::read_csv(paths$IN_NK, show_col_types = FALSE) %>% dplyr::rename(RMSE_NK = RMSE)
  df <- dplyr::inner_join(ok, nk, by = "Year") %>%
    dplyr::filter(!is.na(RMSE_OK), !is.na(RMSE_NK)) %>%
    dplyr::mutate(d = RMSE_OK^2 - RMSE_NK^2)
  n <- nrow(df); dm_t <- mean(df$d)/(sd(df$d)/sqrt(n)); pval <- 2*pt(abs(dm_t), df = n-1, lower.tail = FALSE)
  better <- ifelse(mean(df$d) > 0, "NK", "OK")
  dm_infl <- data.frame(Category="Inflation", n_years=n, mean_MSE_diff=mean(df$d), DM_t=dm_t, p_value=pval, Better=better)
  print(dm_infl); readr::write_excel_csv(dm_infl, "outputs/article/metrics/IN_DM.csv")
}
# Unemployment DM
if (file.exists(paths$UN_OK) && file.exists(paths$UN_NK)) {
  ok <- readr::read_csv(paths$UN_OK, show_col_types = FALSE) %>% dplyr::rename(RMSE_OK = RMSE)
  nk <- readr::read_csv(paths$UN_NK, show_col_types = FALSE) %>% dplyr::rename(RMSE_NK = RMSE)
  df <- dplyr::inner_join(ok, nk, by = "Year") %>%
    dplyr::filter(!is.na(RMSE_OK), !is.na(RMSE_NK)) %>%
    dplyr::mutate(d = RMSE_OK^2 - RMSE_NK^2)
  n <- nrow(df); dm_t <- mean(df$d)/(sd(df$d)/sqrt(n)); pval <- 2*pt(abs(dm_t), df = n-1, lower.tail = FALSE)
  better <- ifelse(mean(df$d) > 0, "NK", "OK")
  dm_unemp <- data.frame(Category="Unemployment", n_years=n, mean_MSE_diff=mean(df$d), DM_t=dm_t, p_value=pval, Better=better)
  print(dm_unemp); readr::write_excel_csv(dm_unemp, "outputs/article/metrics/UN_DM.csv")
}

message("=== TABLE 6 (Diagnostics) ===")
safe_source("code/make_table6_diagnostics.R")

message("=== FIGURES ===")
# Fig1: network effects
lbls <- c(
  W_Lag_Inflation    = "W × Lag(Inflation)",
  W_Inflation        = "W × Inflation",
  W_House_Cons_Share = "W × Household Cons. Share"
)
read_net <- function(model, path, terms_keep, var_labels) {
  stopifnot(file.exists(path))
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::filter(term %in% terms_keep) %>%
    dplyr::mutate(
      Model    = model,
      Variable = var_labels[term],
      lower95  = estimate - 1.96 * std.error,
      upper95  = estimate + 1.96 * std.error
    ) %>%
    dplyr::select(Model, Variable, estimate, std.error, lower95, upper95)
}
paths_coef <- list(
  IN_OK = "outputs/article/models/IN_OK_main_coef_utf8.csv",
  IN_NK = "outputs/article/models/IN_NK_main_coef_utf8.csv",
  UN_OK = "outputs/article/models/UN_OK_main_coef_utf8.csv",
  UN_NK = "outputs/article/models/UN_NK_main_coef_utf8.csv"
)
df_plot <- dplyr::bind_rows(
  read_net("IN_OK", paths_coef$IN_OK, "W_Lag_Inflation", lbls),
  read_net("IN_NK", paths_coef$IN_NK, "W_Lag_Inflation", lbls),
  read_net("UN_OK", paths_coef$UN_OK, "W_Inflation", lbls),
  read_net("UN_NK", paths_coef$UN_NK, c("W_House_Cons_Share","W_Inflation"), lbls)
)
pos <- position_dodge(width = 0.5)
p1 <- ggplot(df_plot, aes(x = Model, y = estimate, color = Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = pos, size = 2.8) +
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.12, position = pos) +
  labs(title = "Network effects: coefficients with 95% CI", x = "", y = "Coefficient") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave("outputs/article/figs/Fig1_network_effects.png", p1, width = 7.5, height = 4.8, dpi = 300)

# Fig2A: Inflation LOYO
if (file.exists(paths$IN_OK) && file.exists(paths$IN_NK)) {
  in_ok <- readr::read_csv(paths$IN_OK, show_col_types = FALSE) %>% dplyr::mutate(Model = "OK")
  in_nk <- readr::read_csv(paths$IN_NK, show_col_types = FALSE) %>% dplyr::mutate(Model = "NK")
  df_infl <- dplyr::bind_rows(in_ok, in_nk) %>% dplyr::mutate(Year = as.integer(Year)) %>% dplyr::filter(!is.na(RMSE))
  p2a <- ggplot(df_infl, aes(x = Year, y = RMSE, color = Model)) +
    geom_line(size = 0.9) + geom_point(size = 2.2) +
    scale_x_continuous(breaks = sort(unique(df_infl$Year))) +
    labs(title = "LOYO RMSE by Year — Inflation (OK vs NK)", x = "", y = "RMSE") +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  ggsave("outputs/article/figs/Fig2A_inflation_LOYO.png", p2a, width = 7.5, height = 4.8, dpi = 300)
}

# Fig2B: Unemployment LOYO
if (file.exists(paths$UN_OK) && file.exists(paths$UN_NK)) {
  un_ok <- readr::read_csv(paths$UN_OK, show_col_types = FALSE) %>% dplyr::mutate(Model = "OK")
  un_nk <- readr::read_csv(paths$UN_NK, show_col_types = FALSE) %>% dplyr::mutate(Model = "NK")
  df_unemp <- dplyr::bind_rows(un_ok, un_nk) %>% dplyr::mutate(Year = as.integer(Year)) %>% dplyr::filter(!is.na(RMSE))
  p2b <- ggplot(df_unemp, aes(x = Year, y = RMSE, color = Model)) +
    geom_line(size = 0.9) + geom_point(size = 2.2) +
    scale_x_continuous(breaks = sort(unique(df_unemp$Year))) +
    labs(title = "LOYO RMSE by Year — Unemployment (OK vs NK)", x = "", y = "RMSE") +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  ggsave("outputs/article/figs/Fig2B_unemployment_LOYO.png", p2b, width = 7.5, height = 4.8, dpi = 300)
}

message("=== sessionInfo ===")
info <- capture.output(sessionInfo())
writeLines(info, "outputs/article/sessionInfo.txt")

message("All done. Outputs in outputs/article/")