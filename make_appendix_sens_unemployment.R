# code/make_appendix_sens_unemployment.R — Summary of unemployment sensitivities
source("code/utils.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr); library(writexl) })
dir.create("outputs/article/tables", recursive = TRUE, showWarnings = FALSE)

paths <- list(
  UN_OK_main          = "outputs/article/models/UN_OK_main_coef_utf8.csv",
  UN_OK_sens_Services = "outputs/article/models/UN_OK_sens_Services_coef_utf8.csv",
  UN_NK_main          = "outputs/article/models/UN_NK_main_coef_utf8.csv",
  UN_NK_sens_no_WHOUSE= "outputs/article/models/UN_NK_sens_no_WHOUSE_coef_utf8.csv",
  UN_NK_sens_no_WINF  = "outputs/article/models/UN_NK_sens_no_WINF_coef_utf8.csv"
)

keep <- c(
  "Lag_Unemployment",
  "Inflation","W_Inflation",
  "House_Cons_Share","W_House_Cons_Share",
  "Economic_Participation_Rate",
  "Industry_Share_of_GDP","Services_GDP_Share",
  "Natural_Disasters"
)

labels <- c(
  Lag_Unemployment="Lag(Unemployment)",
  Inflation="Inflation",
  W_Inflation="W × Inflation",
  House_Cons_Share="Household Consumption Share",
  W_House_Cons_Share="W × Household Consumption Share",
  Economic_Participation_Rate="Economic Participation Rate",
  Industry_Share_of_GDP="Industry Share of GDP",
  Services_GDP_Share="Services Share of GDP",
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

# ترتیب نمایش ستون‌ها
order_ms <- c("UN_OK_main","UN_OK_sens_Services","UN_NK_main","UN_NK_sens_no_WHOUSE","UN_NK_sens_no_WINF")
tbl$ModelSpec <- factor(tbl$ModelSpec, levels = order_ms)

tbl_un <- tbl %>%
  select(ModelSpec, Variable, `Estimate (SE)`) %>%
  pivot_wider(names_from = ModelSpec, values_from = `Estimate (SE)`)

print(tbl_un)

# ذخیره
out_csv  <- "outputs/article/tables/Appendix_Sensitivity_Unemployment.csv"
out_xlsx <- "outputs/article/tables/Appendix_Sensitivity_Unemployment.xlsx"
readr::write_excel_csv(tbl_un, out_csv)
writexl::write_xlsx(tbl_un, out_xlsx)
cat("Saved unemployment sensitivity table to:\n -", out_csv, "\n -", out_xlsx, "\n")