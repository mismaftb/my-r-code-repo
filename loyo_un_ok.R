# code/loyo_un_ok.R — LOYO RMSE for UN_OK
source("code/utils.R")
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

# داده و مشخصه مدل بیکاری OK را آماده می‌کنیم
source("code/un_ok.R")  # می‌سازد: unok_cc, p_unok_cc, fmla_un_ok

df_all <- as.data.frame(p_unok_cc)
years  <- sort(unique(df_all$Year_num))

loyo_rows <- list()

for (tt in years) {
  train_idx <- which(df_all$Year_num <= (tt - 1))
  test_idx  <- which(df_all$Year_num == tt)
  
  if (length(train_idx) < 10 || length(test_idx) != length(province_levels)) {
    loyo_rows[[as.character(tt)]] <- data.frame(Year = tt, RMSE = NA_real_)
    next
  }
  
  p_train <- pdata.frame(df_all[train_idx, ], index = c("Province_Name","Year"), drop.index = FALSE)
  p_test  <- df_all[test_idx, , drop = FALSE]
  
  fit_tr <- try(spgm(
    fmla_un_ok,
    data            = p_train,
    index           = c("Province_Name","Year"),
    listw           = Wlist,
    model           = "within",
    lag             = FALSE,
    spatial.error   = FALSE,
    method          = "g2sls",
    moments         = "fullweights",
    endog           = ~ Lag_Unemployment,
    instruments     = ~ L2_Unemployment + L3_Unemployment +
      Economic_Participation_Rate + Industry_Share_of_GDP + Natural_Disasters,
    lag.instruments = FALSE,
    verbose         = FALSE
  ), silent = TRUE)
  
  if (inherits(fit_tr, "try-error")) {
    loyo_rows[[as.character(tt)]] <- data.frame(Year = tt, RMSE = NA_real_)
    next
  }
  
  beta <- coef(fit_tr)
  Xte  <- model.matrix(fmla_un_ok, data = p_test)
  cn   <- intersect(colnames(Xte), names(beta))
  yhat <- as.numeric(Xte[, cn, drop = FALSE] %*% beta[cn])
  yobs <- p_test$Unemployment
  
  rmse <- sqrt(mean((yobs - yhat)^2, na.rm = TRUE))
  loyo_rows[[as.character(tt)]] <- data.frame(Year = tt, RMSE = rmse)
}

loyo_tbl <- do.call(rbind, loyo_rows)
print(loyo_tbl)

readr::write_excel_csv(loyo_tbl, "outputs/article/metrics/UN_OK_LOYO_RMSE.csv")