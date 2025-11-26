# code/make_appendix_sens_inflation.R — Summary of inflation sensitivities
source("code/utils.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(writexl) })
dir.create("outputs/article/tables", recursive = TRUE, showWarnings = FALSE)

# مسیر ضرایب
paths <- list(
  IN_OK_main     = "outputs/article/models/IN_OK_main_coef_utf8.csv",
  IN_OK_sens_Liq = "outputs/article/models/IN_OK_sens_Liq_coef_utf8.csv",
  IN_OK_sens_noW = "outputs/article/models/IN_OK_sens_noW_coef_utf8.csv",
  IN_NK_main     = "outputs/article/models/IN_NK_main_coef_utf8.csv",
  IN_NK_sens_Liq = "outputs/article/models/IN_NK_sens_Liq_coef_utf8.csv",
  IN_NK_sens_noW = "outputs/article/models/IN_NK_sens_noW_coef_utf8.csv"
)

keep <- c(
  "Lag_Inflation","W_Lag_Inflation","Exp_L1",
  "Out_Pot_Gap",
  "Interest_Rate","Liquidity_Growth_Rate",
  "Currency_Price_Growth_Rate","Gasoline_Price_Growth_Rate",
  "Sanctions_Index","Natural_Disasters"
)
labels <- c(
  Lag_Inflation="Lag(Inflation)",
  W_Lag_Inflation="W × Lag(Inflation)",
  Exp_L1="Expected Inflation (t−1)",
  Out_Pot_Gap="Output–Potential Gap",
  Interest_Rate="Interest Rate",
  Liquidity_Growth_Rate="Liquidity Growth",
  Currency_Price_Growth_Rate="Currency Growth",
  Gasoline_Price_Growth_Rate="Gasoline Price Growth",
  Sanctions_Index="Sanctions Index",
  Natural_Disasters="Natural Disasters"
)
star <- function(p){ if (is.na(p)) "" else if (p<0.001) "***" else if (p<0.01) "**" else if (p<0.05) "*" else if (p<0.1) "." else "" }

read_one <- function(name, path){
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) %>%
    filter(term %in% keep) %>%
    mutate(
      ModelSpec = name,
      Variable  = labels[term],
      Sig       = vapply(p.value, star, character(1)),
      Estimate  = round(estimate, 3),
      Std_Error = round(std.error, 3),
      `Estimate (SE)` = paste0(Estimate, " (", Std_Error, ")", Sig)
    ) %>%
    select(ModelSpec, term, Variable, Estimate, Std_Error, p.value, `Estimate (SE)`)
}

tbl <- bind_rows(mapply(read_one, names(paths), paths, SIMPLIFY = FALSE))
# ترتیب نمایش: اول OK سپس NK
order_ms <- c("IN_OK_main","IN_OK_sens_Liq","IN_OK_sens_noW","IN_NK_main","IN_NK_sens_Liq","IN_NK_sens_noW")
tbl$ModelSpec <- factor(tbl$ModelSpec, levels = order_ms)
tbl <- arrange(tbl, ModelSpec, match(term, keep))

# خروجی تمیز شده برای ارائه
tbl_inf <- tbl %>%
  select(ModelSpec, Variable, `Estimate (SE)`) %>%
  tidyr::pivot_wider(names_from = ModelSpec, values_from = `Estimate (SE)`)

print(tbl_inf)

# ذخیره
out_csv  <- "outputs/article/tables/Appendix_Sensitivity_Inflation.csv"
out_xlsx <- "outputs/article/tables/Appendix_Sensitivity_Inflation.xlsx"
readr::write_excel_csv(tbl_inf, out_csv)
writexl::write_xlsx(tbl_inf, out_xlsx)
cat("Saved inflation sensitivity table to:\n -", out_csv, "\n -", out_xlsx, "\n")