# code/loyo_in_ok.R — LOYO RMSE for IN_OK
source("code/utils.R")
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

# داده و مشخصه مدل تورم OK را آماده می‌کنیم
source("code/in_ok.R")  # می‌سازد: inok_cc, p_inok_cc, fmla_in_ok

df_all <- as.data.frame(p_inok_cc)
years  <- sort(unique(df_all$Year_num))

loyo_rows <- list()

for (tt in years) {
  train_idx <- which(df_all$Year_num <= (tt - 1))
  test_idx  <- which(df_all$Year_num == tt)
  
  # اگر داده آموزش کم بود، رد شو
  if (length(train_idx) < 10 || length(test_idx) != length(province_levels)) {
    loyo_rows[[as.character(tt)]] <- data.frame(Year = tt, RMSE = NA_real_)
    next
  }
  
  p_train <- pdata.frame(df_all[train_idx, ], index = c("Province_Name","Year"), drop.index = FALSE)
  p_test  <- df_all[test_idx, , drop = FALSE]
  
  # برازش مدل روی آموزش (همان مشخصه مقاله)
  fit_tr <- try(spgm(
    fmla_in_ok,
    data            = p_train,
    index           = c("Province_Name","Year"),
    listw           = Wlist,
    model           = "within",
    lag             = FALSE,
    spatial.error   = FALSE,
    method          = "g2sls",
    moments         = "fullweights",
    endog           = ~ Lag_Inflation + W_Lag_Inflation,
    instruments     = ~ L2_Inflation + L3_Inflation + W_L2_Inflation + W_L3_Inflation + Out_Pot_Gap + Natural_Disasters,
    lag.instruments = FALSE,
    verbose         = FALSE
  ), silent = TRUE)
  
  if (inherits(fit_tr, "try-error")) {
    loyo_rows[[as.character(tt)]] <- data.frame(Year = tt, RMSE = NA_real_)
    next
  }
  
  beta <- coef(fit_tr)
  Xte  <- model.matrix(fmla_in_ok, data = p_test)
  cn   <- intersect(colnames(Xte), names(beta))
  yhat <- as.numeric(Xte[, cn, drop = FALSE] %*% beta[cn])
  yobs <- p_test$Inflation
  
  rmse <- sqrt(mean((yobs - yhat)^2, na.rm = TRUE))
  loyo_rows[[as.character(tt)]] <- data.frame(Year = tt, RMSE = rmse)
}

loyo_tbl <- do.call(rbind, loyo_rows)
print(loyo_tbl)

readr::write_excel_csv(loyo_tbl, "outputs/article/metrics/IN_OK_LOYO_RMSE.csv")