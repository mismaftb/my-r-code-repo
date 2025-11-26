# code/make_table2_in_nk.R — Table 2: IN_NK (ضرایب اصلی)
source("code/utils.R")

path_coef <- "outputs/article/models/IN_NK_main_coef_utf8.csv"
stopifnot(file.exists(path_coef))
df <- readr::read_csv(path_coef, show_col_types = FALSE)

# فقط متغیرهای اصلی (OFE حذف)
keep <- c("Lag_Inflation","W_Lag_Inflation","Exp_L1",
          "Out_Pot_Gap",
          "Interest_Rate","Currency_Price_Growth_Rate",
          "Gasoline_Price_Growth_Rate","Sanctions_Index","Natural_Disasters")
df <- df %>% dplyr::filter(term %in% keep)

# برچسب‌ها
labels <- c(
  Lag_Inflation = "Lag(Inflation)",
  W_Lag_Inflation = "W × Lag(Inflation)",
  Exp_L1 = "Expected Inflation (t−1)",
  Out_Pot_Gap = "Output–Potential Gap",
  Interest_Rate = "Interest Rate",
  Currency_Price_Growth_Rate = "Currency Growth",
  Gasoline_Price_Growth_Rate = "Gasoline Price Growth",
  Sanctions_Index = "Sanctions Index",
  Natural_Disasters = "Natural Disasters"
)

star <- function(p){
  if (is.na(p)) "" else if (p<0.001) "***" else if (p<0.01) "**" else if (p<0.05) "*" else if (p<0.1) "." else ""
}

tbl2 <- df %>%
  dplyr::mutate(
    Variable = labels[term],
    Sig      = vapply(p.value, star, character(1)),
    Estimate = round(estimate, 3),
    Std_Error= round(std.error, 3),
    `Estimate (SE)` = paste0(Estimate, " (", Std_Error, ")", Sig)
  ) %>%
  dplyr::select(Variable, Estimate, Std_Error, p.value, `Estimate (SE)`) %>%
  dplyr::arrange(factor(Variable, levels = labels[keep]))

print(tbl2)

# ذخیره
out_csv  <- "outputs/article/tables/Table2_IN_NK.csv"
out_xlsx <- "outputs/article/tables/Table2_IN_NK.xlsx"
readr::write_excel_csv(tbl2, out_csv)
writexl::write_xlsx(tbl2, out_xlsx)
cat("Saved Table 2 to:\n -", out_csv, "\n -", out_xlsx, "\n")