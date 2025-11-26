# code/loyo_in_nk.R — LOYO RMSE for IN_NK
source("code/utils.R")
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

# داده و مشخصه مدل تورم NK را آماده می‌کنیم
source("code/in_nk.R")  # می‌سازد: innk_cc, p_innk_cc, fmla_in_nk

df_all <- as.data.frame(p_innk_cc)
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
    fmla_in_nk,
    data            = p_train,
    index           = c("Province_Name","Year"),
    listw           = Wlist,
    model           = "within",
    lag             = FALSE,
    spatial.error   = FALSE,
    method          = "g2sls",
    moments         = "fullweights",
    endog           = ~ Lag_Inflation + W_Lag_Inflation,
    instruments     = ~ L2_Inflation + L3_Inflation +
      W_L2_Inflation + W_L3_Inflation +
      Out_Pot_Gap + Natural_Disasters,
    lag.instruments = FALSE,
    verbose         = FALSE
  ), silent = TRUE)
  
  if (inherits(fit_tr, "try-error")) {
    loyo_rows[[as.character(tt)]] <- data.frame(Year = tt, RMSE = NA_real_)
    next
  }
  
  beta <- coef(fit_tr)
  Xte  <- model.matrix(fmla_in_nk, data = p_test)
  cn   <- intersect(colnames(Xte), names(beta))
  yhat <- as.numeric(Xte[, cn, drop = FALSE] %*% beta[cn])
  yobs <- p_test$Inflation
  
  rmse <- sqrt(mean((yobs - yhat)^2, na.rm = TRUE))
  loyo_rows[[as.character(tt)]] <- data.frame(Year = tt, RMSE = rmse)
}

loyo_tbl <- do.call(rbind, loyo_rows)
print(loyo_tbl)

readr::write_excel_csv(loyo_tbl, "outputs/article/metrics/IN_NK_LOYO_RMSE.csv")