# code/make_table1_in_ok.R — Table 1: IN_OK (ضرایب اصلی)
source("code/utils.R")

# خواندن ضرایب ذخیره‌شده
path_coef <- "outputs/article/models/IN_OK_main_coef_utf8.csv"
stopifnot(file.exists(path_coef))
df <- readr::read_csv(path_coef, show_col_types = FALSE)

# نگه‌داشتن فقط متغیرهای اصلی (OFE حذف شود)
keep <- c("Lag_Inflation","W_Lag_Inflation","Out_Pot_Gap",
          "Interest_Rate","Currency_Price_Growth_Rate",
          "Gasoline_Price_Growth_Rate","Sanctions_Index","Natural_Disasters")
df <- df %>% filter(term %in% keep)

# برچسب‌های خوانا
labels <- c(
  Lag_Inflation = "Lag(Inflation)",
  W_Lag_Inflation = "W × Lag(Inflation)",
  Out_Pot_Gap = "Output–Potential Gap",
  Interest_Rate = "Interest Rate",
  Currency_Price_Growth_Rate = "Currency Growth",
  Gasoline_Price_Growth_Rate = "Gasoline Price Growth",
  Sanctions_Index = "Sanctions Index",
  Natural_Disasters = "Natural Disasters"
)

# کد معناداری
star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  if (p < 0.1)   return(".")
  ""
}

tbl1 <- df %>%
  mutate(
    Variable = labels[term],
    Sig      = vapply(p.value, star, character(1)),
    Estimate = round(estimate, 3),
    Std_Error= round(std.error, 3),
    `Estimate (SE)` = paste0(Estimate, " (", Std_Error, ")", Sig)
  ) %>%
  select(Variable, Estimate, Std_Error, p.value, `Estimate (SE)`) %>%
  arrange(factor(Variable, levels = labels[keep]))

print(tbl1)

# ذخیره جدول
out_csv <- "outputs/article/tables/Table1_IN_OK.csv"
out_xlsx<- "outputs/article/tables/Table1_IN_OK.xlsx"
readr::write_excel_csv(tbl1, out_csv)
writexl::write_xlsx(tbl1, out_xlsx)
cat("Saved Table 1 to:\n -", out_csv, "\n -", out_xlsx, "\n")