# code/in_ok.R — Old Keynesian Inflation (SDPD hybrid: y_{t-1} + W y_{t-1} + OutGap + policy/shocks)
source("code/utils.R")

# 1) وزن فضایی
Wlist <- read_Wlist_safe(
  rds_path = "weights/Wlist.rds",
  wpath    = "weights/spatial_weight_matrix_final.xlsx"
)

# 2) خواندن داده تورم OK
raw <- readxl::read_excel("data/IN_OK.xlsx")

inok <- raw %>%
  transmute(
    Province_Name = factor(trimws(as.character(Province_Name)), levels = province_levels),
    Year          = as.integer(Year),
    Year_num      = as.integer(Year),
    # وابسته و کلیدی‌ها
    Inflation     = as.numeric(Inflation),
    Out_Pot_Gap   = as.numeric(Out_Pot_Gap),
    Interest_Rate = as.numeric(Interest_Rate),
    Currency_Price_Growth_Rate  = as.numeric(Currency_Price_Growth_Rate),
    Gasoline_Price_Growth_Rate  = as.numeric(Gasoline_Price_Growth_Rate),
    Sanctions_Index             = as.numeric(Sanctions_Index),
    Natural_Disasters           = as.numeric(Natural_Disasters),
    Liquidity_Growth_Rate       = as.numeric(Liquidity_Growth_Rate)  # برای OFE
  ) %>%
  arrange(Province_Name, Year)

# 3) وقفه‌های تورم برای پویایی/ابزارها
# از plm::lag روی pdata.frame استفاده می‌کنیم تا درون‌استانی محاسبه شود
p_inok <- pdata.frame(inok, index = c("Province_Name","Year"), drop.index = FALSE)
inok$Lag_Inflation <- as.numeric(plm::lag(p_inok$Inflation, 1))
inok$L2_Inflation  <- as.numeric(plm::lag(p_inok$Inflation, 2))
inok$L3_Inflation  <- as.numeric(plm::lag(p_inok$Inflation, 3))

# 4) OFE زمان نسبت به 5 سری ملی
ofe_obj <- make_OFE(
  inok,
  year_var = "Year_num",
  series   = c("Interest_Rate","Currency_Price_Growth_Rate",
               "Gasoline_Price_Growth_Rate","Sanctions_Index","Liquidity_Growth_Rate")
)
inok <- attach_OFE(inok, ofe_obj, year_var = "Year_num")
ofe_cols <- grep("^OFE_", names(inok), value = TRUE)

# 5) ساخت W-وقفه برای y_{t-1} و ابزارهای عمیق
inok <- add_W_by_year(inok, Wlist, vars = c("Lag_Inflation","L2_Inflation","L3_Inflation"), year_var = "Year_num")

# 6) برش برای ابزارهای بدون NA (از 2004 به بعد) و بازسازی پنل
inok_cc <- inok %>%
  filter(Year_num >= 2004) %>%
  filter(!is.na(L2_Inflation) & !is.na(L3_Inflation))

p_inok_cc <- pdata.frame(inok_cc, index = c("Province_Name","Year"), drop.index = FALSE)

# 7) مشخصه مقاله: SDPD هیبرید (y_{t-1} + W y_{t-1} + OutGap + policy/shocks + OFE)
rhs <- c(
  "Lag_Inflation", "W_Lag_Inflation",   # پویایی زمانی و سرریز زمانی-فضایی
  "Out_Pot_Gap",                         # فشار تقاضا
  "Interest_Rate", "Currency_Price_Growth_Rate", "Gasoline_Price_Growth_Rate", "Sanctions_Index",
  "Natural_Disasters",
  ofe_cols
)
fmla_in_ok <- as.formula(paste("Inflation ~", paste(rhs, collapse = " + ")))

fit_in_ok <- spgm(
  fmla_in_ok,
  data            = p_inok_cc,
  index           = c("Province_Name","Year"),
  listw           = Wlist,
  model           = "within",     # FE استانی
  lag             = FALSE,        # Wy_it نمی‌آوریم (از W y_{t-1} استفاده شده)
  spatial.error   = FALSE,
  method          = "g2sls",
  moments         = "fullweights",
  endog           = ~ Lag_Inflation + W_Lag_Inflation,     # هر دو درون‌زا
  instruments     = ~ L2_Inflation + L3_Inflation +        # ابزارهای y
    W_L2_Inflation + W_L3_Inflation +     # ابزارهای فضاییِ y
    Out_Pot_Gap + Natural_Disasters,      # Xهای استانی (ساده و امن)
  lag.instruments = FALSE,
  verbose         = TRUE
)

print(summary(fit_in_ok))
save_model_outputs(fit_in_ok, "IN_OK_main")  # ذخیره به outputs/article/models/

