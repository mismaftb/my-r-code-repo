# code/make_table6_diagnostics.R — Table 6: Diagnostics (First-stage F + Moran's I)
source("code/utils.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(writexl) })

paths_f <- list(
  IN_OK = "outputs/article/metrics/IN_OK_firststage_F.csv",
  IN_NK = "outputs/article/metrics/IN_NK_firststage_F.csv",
  UN_OK = "outputs/article/metrics/UN_OK_firststage_F.csv",
  UN_NK = "outputs/article/metrics/UN_NK_firststage_F.csv"
)
paths_m <- list(
  IN_OK = "outputs/article/metrics/IN_OK_moran_by_year.csv",
  IN_NK = "outputs/article/metrics/IN_NK_moran_by_year.csv",
  UN_OK = "outputs/article/metrics/UN_OK_moran_by_year.csv",
  UN_NK = "outputs/article/metrics/UN_NK_moran_by_year.csv"
)

mk_row <- function(model){
  fpath <- paths_f[[model]]; mpath <- paths_m[[model]]
  Fy <- Fw <- py <- pw <- NA_real_
  if (file.exists(fpath)) {
    ff <- read_csv(fpath, show_col_types = FALSE)
    if ("Endogenous" %in% names(ff)) {
      if (any(grepl("Lag_Inflation|Lag_Unemployment", ff$Endogenous))) {
        r <- ff %>% filter(grepl("Lag_Inflation|Lag_Unemployment", Endogenous)) %>% slice(1)
        Fy <- r$F_stat; py <- r$p_value
      }
      if (any(grepl("W_Lag_Inflation", ff$Endogenous))) {
        r2 <- ff %>% filter(grepl("W_Lag_Inflation", Endogenous)) %>% slice(1)
        Fw <- r2$F_stat; pw <- r2$p_value
      }
    }
  }
  n_years <- n_sig10 <- NA_integer_; pmin <- NA_real_
  if (file.exists(mpath)) {
    mm <- read_csv(mpath, show_col_types = FALSE)
    n_years <- sum(!is.na(mm$p.value))
    n_sig10 <- sum(mm$p.value < 0.1, na.rm = TRUE)
    pmin <- suppressWarnings(min(mm$p.value, na.rm = TRUE))
  }
  data.frame(
    Model = model,
    F_y = Fy, p_y = py,
    F_Wy = Fw, p_Wy = pw,
    Moran_n_years = n_years,
    Moran_sig_p_lt_0_1 = n_sig10,
    Moran_min_p = pmin
  )
}

tbl6 <- bind_rows(lapply(names(paths_f), mk_row))
print(tbl6)

out_csv  <- "outputs/article/tables/Table6_Diagnostics.csv"
out_xlsx <- "outputs/article/tables/Table6_Diagnostics.xlsx"
readr::write_excel_csv(tbl6, out_csv)
writexl::write_xlsx(tbl6, out_xlsx)
cat("Saved Table 6 to:\n -", out_csv, "\n -", out_xlsx, "\n")