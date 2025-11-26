# code/in_nk.R — New Keynesian Inflation (SDPD هیبرید: y_{t-1} + W y_{t-1} + Exp_L1 + OutGap + policy/shocks)
source("code/utils.R")

# 1) وزن فضایی
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

# 2) خواندن داده تورم NK
raw <- readxl::read_excel("data/IN_NK.xlsx")

innk <- raw %>%
  transmute(
    Province_Name = factor(trimws(as.character(Province_Name)), levels = province_levels),
    Year          = as.integer(Year),
    Year_num      = as.integer(Year),
    # وابسته و کلیدی‌ها
    Inflation     = as.numeric(Inflation),
    Out_Pot_Gap   = as.numeric(Out_Pot_Gap),
    Inflation_Expectations_Index = as.numeric(Inflation_Expectations_Index),
    # سری‌های ملی برای OFE
    Interest_Rate              = as.numeric(Interest_Rate),
    Currency_Price_Growth_Rate = as.numeric(Currency_Price_Growth_Rate),
    Gasoline_Price_Growth_Rate = as.numeric(Gasoline_Price_Growth_Rate),
    Sanctions_Index            = as.numeric(Sanctions_Index),
    Liquidity_Growth_Rate      = as.numeric(Liquidity_Growth_Rate),
    # شوک محلی
    Natural_Disasters          = as.numeric(Natural_Disasters)
  ) %>%
  arrange(Province_Name, Year)

# 3) وقفه‌ها و انتظارات
p_in <- pdata.frame(innk, index = c("Province_Name","Year"), drop.index = FALSE)
innk$Lag_Inflation <- as.numeric(plm::lag(p_in$Inflation, 1))
innk$L2_Inflation  <- as.numeric(plm::lag(p_in$Inflation, 2))
innk$L3_Inflation  <- as.numeric(plm::lag(p_in$Inflation, 3))
innk$Exp_t         <- innk$Inflation_Expectations_Index
innk$Exp_L1        <- as.numeric(plm::lag(p_in$Inflation_Expectations_Index, 1))

# 4) OFE نسبت به ۵ سری ملی
ofe_obj_innk <- make_OFE(
  innk, year_var = "Year_num",
  series = c("Interest_Rate","Currency_Price_Growth_Rate",
             "Gasoline_Price_Growth_Rate","Sanctions_Index","Liquidity_Growth_Rate")
)
innk <- attach_OFE(innk, ofe_obj_innk, year_var = "Year_num")
ofe_cols_innk <- grep("^OFE_", names(innk), value = TRUE)

# 5) ساخت W-وقفه‌های y (برای y_{t-1} و ابزارهای عمیق)
innk <- add_W_by_year(innk, Wlist, vars = c("Lag_Inflation","L2_Inflation","L3_Inflation"), year_var = "Year_num")

# 6) برش برای ابزارهای بدون NA (2004+) و پنل
innk_cc <- innk %>%
  filter(Year_num >= 2004) %>%
  filter(!is.na(L2_Inflation) & !is.na(L3_Inflation))
p_innk_cc <- pdata.frame(innk_cc, index = c("Province_Name","Year"), drop.index = FALSE)

# 7) مشخصه مقاله: SDPD هیبرید NK
rhs_in_nk <- c(
  "Lag_Inflation", "W_Lag_Inflation",       # پویایی زمانی و سرریز زمانی-فضایی
  "Exp_L1",                                  # جزء انتظارات هیبرید
  "Out_Pot_Gap",                             # فشار تقاضا
  "Interest_Rate", "Currency_Price_Growth_Rate",
  "Gasoline_Price_Growth_Rate", "Sanctions_Index",
  "Natural_Disasters",
  ofe_cols_innk
)
fmla_in_nk <- as.formula(paste("Inflation ~", paste(rhs_in_nk, collapse = " + ")))

fit_in_nk <- spgm(
  fmla_in_nk,
  data            = p_innk_cc,
  index           = c("Province_Name","Year"),
  listw           = Wlist,
  model           = "within",
  lag             = FALSE,    # Wy_it نمی‌آوریم (از W y_{t-1} استفاده شده)
  spatial.error   = FALSE,
  method          = "g2sls",
  moments         = "fullweights",
  endog           = ~ Lag_Inflation + W_Lag_Inflation,  # درون‌زاها
  instruments     = ~ L2_Inflation + L3_Inflation +     # ابزارهای y
    W_L2_Inflation + W_L3_Inflation +  # ابزارهای فضایی y
    Out_Pot_Gap + Natural_Disasters,   # Xهای استانی امن
  lag.instruments = FALSE,
  verbose         = TRUE
)

print(summary(fit_in_nk))
save_model_outputs(fit_in_nk, "IN_NK_main")