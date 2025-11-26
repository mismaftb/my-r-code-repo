# code/make_table4_un_nk.R — Table 4: UN_NK (ضرایب اصلی)
source("code/utils.R")

path_coef <- "outputs/article/models/UN_NK_main_coef_utf8.csv"
stopifnot(file.exists(path_coef))
df <- readr::read_csv(path_coef, show_col_types = FALSE)

# فقط متغیرهای اصلی (OFE حذف)
keep <- c("Lag_Unemployment",
          "House_Cons_Share","W_House_Cons_Share","W_Inflation",
          "Economic_Participation_Rate","Industry_Share_of_GDP","Natural_Disasters")
df <- dplyr::filter(df, term %in% keep)

# برچسب‌ها
labels <- c(
  Lag_Unemployment = "Lag(Unemployment)",
  House_Cons_Share = "Household Consumption Share",
  W_House_Cons_Share = "W × Household Consumption Share",
  W_Inflation = "W × Inflation",
  Economic_Participation_Rate = "Economic Participation Rate",
  Industry_Share_of_GDP = "Industry Share of GDP",
  Natural_Disasters = "Natural Disasters"
)

star <- function(p){
  if (is.na(p)) "" else if (p<0.001) "***" else if (p<0.01) "**" else if (p<0.05) "*" else if (p<0.1) "." else ""
}

tbl4 <- df %>%
  dplyr::mutate(
    Variable = labels[term],
    Sig      = vapply(p.value, star, character(1)),
    Estimate = round(estimate, 3),
    Std_Error= round(std.error, 3),
    `Estimate (SE)` = paste0(Estimate, " (", Std_Error, ")", Sig)
  ) %>%
  dplyr::select(Variable, Estimate, Std_Error, p.value, `Estimate (SE)`) %>%
  dplyr::arrange(factor(Variable, levels = labels[keep]))

print(tbl4)

# ذخیره
out_csv  <- "outputs/article/tables/Table4_UN_NK.csv"
out_xlsx <- "outputs/article/tables/Table4_UN_NK.xlsx"
readr::write_excel_csv(tbl4, out_csv)
writexl::write_xlsx(tbl4, out_xlsx)
cat("Saved Table 4 to:\n -", out_csv, "\n -", out_xlsx, "\n")