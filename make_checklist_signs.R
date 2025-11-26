# code/make_checklist_signs.R
source("code/utils.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(writexl) })

paths <- list(
  IN_OK = "outputs/article/models/IN_OK_main_coef_utf8.csv",
  IN_NK = "outputs/article/models/IN_NK_main_coef_utf8.csv",
  UN_OK = "outputs/article/models/UN_OK_main_coef_utf8.csv",
  UN_NK = "outputs/article/models/UN_NK_main_coef_utf8.csv"
)

exp_sign <- list(
  IN_OK = c(
    Lag_Inflation="pos", W_Lag_Inflation="pos",
    Out_Pot_Gap="pos", Interest_Rate="neg",
    Currency_Price_Growth_Rate="pos", Gasoline_Price_Growth_Rate="pos",
    Sanctions_Index="pos", Natural_Disasters="pos"
  ),
  IN_NK = c(
    Lag_Inflation="pos", W_Lag_Inflation="pos", Exp_L1="pos",
    Out_Pot_Gap="pos", Interest_Rate="neg",
    Currency_Price_Growth_Rate="pos", Gasoline_Price_Growth_Rate="pos",
    Sanctions_Index="pos", Natural_Disasters="pos"
  ),
  UN_OK = c(
    Lag_Unemployment="pos", Inflation="neg", W_Inflation="neg",
    Economic_Participation_Rate="ambig",
    Industry_Share_of_GDP="neg", Natural_Disasters="pos"
  ),
  UN_NK = c(
    Lag_Unemployment="pos",
    House_Cons_Share="neg", W_House_Cons_Share="neg", W_Inflation="neg",
    Economic_Participation_Rate="ambig",
    Industry_Share_of_GDP="neg", Natural_Disasters="pos"
  )
)

labels <- c(
  Lag_Inflation="Lag(Inflation)",
  W_Lag_Inflation="W × Lag(Inflation)",
  Out_Pot_Gap="Output–Potential Gap",
  Interest_Rate="Interest Rate",
  Currency_Price_Growth_Rate="Currency Growth",
  Gasoline_Price_Growth_Rate="Gasoline Price Growth",
  Sanctions_Index="Sanctions Index",
  Natural_Disasters="Natural Disasters",
  Exp_L1="Expected Inflation (t−1)",
  Lag_Unemployment="Lag(Unemployment)",
  Inflation="Inflation",
  W_Inflation="W × Inflation",
  Economic_Participation_Rate="Economic Participation Rate",
  Industry_Share_of_GDP="Industry Share of GDP",
  House_Cons_Share="Household Consumption Share",
  W_House_Cons_Share="W × Household Consumption Share"
)

star <- function(p){ if (is.na(p)) "" else if (p<0.001) "***" else if (p<0.01) "**" else if (p<0.05) "*" else if (p<0.1) "." else "" }

rows <- list()
for (m in names(paths)) {
  if (!file.exists(paths[[m]])) next
  df <- read_csv(paths[[m]], show_col_types = FALSE)
  keep <- intersect(names(exp_sign[[m]]), df$term)
  tmp <- df %>%
    filter(term %in% keep) %>%
    mutate(
      Variable = labels[term],
      Expected = exp_sign[[m]][term],
      Sign     = ifelse(estimate > 0, "pos", ifelse(estimate < 0, "neg", "zero")),
      Sig      = vapply(p.value, star, character(1)),
      SignOK   = dplyr::case_when(
        Expected %in% c("pos","neg") & Sign == Expected ~ "✓",
        Expected %in% c("pos","neg") & Sign != Expected ~ "✗",
        Expected == "ambig" ~ "—",
        TRUE ~ ""
      ),
      Estimate = round(estimate, 3),
      Std_Error= round(std.error, 3),
      `Estimate (SE)` = paste0(Estimate, " (", Std_Error, ")", Sig)
    ) %>%
    mutate(Model = m) %>%
    select(Model, Variable, Expected, Sign, SignOK, p.value, `Estimate (SE)`)
  rows[[m]] <- tmp
}
chk <- bind_rows(rows) %>% arrange(Model, Variable)

print(chk)
readr::write_excel_csv(chk, "outputs/article/tables/Checklist_Signs.csv")
writexl::write_xlsx(chk, "outputs/article/tables/Checklist_Signs.xlsx")
cat("Saved Checklist_Signs to outputs/article/tables/\n")