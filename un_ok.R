# code/un_ok.R — Old Keynesian Unemployment
# Dynamic SLX (spgm): y_{t-1} + Inflation + W_Inflation + EPR + Structure + ND + OFE
source("code/utils.R")

# 1) وزن فضایی
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

# 2) خواندن داده بیکاری OK (فایل باید شامل این ستون‌ها باشد)
# Province_Name, Year, Unemployment, Inflation,
# Economic_Participation_Rate, Industry_Share_of_GDP (یا Services_GDP_Share),
# Interest_Rate, Currency_Price_Growth_Rate, Gasoline_Price_Growth_Rate, Sanctions_Index, Liquidity_Growth_Rate,
# Natural_Disasters
raw <- readxl::read_excel("data/UN_OK.xlsx")

unok <- raw %>%
  transmute(
    Province_Name = factor(trimws(as.character(Province_Name)), levels = province_levels),
    Year          = as.integer(Year),
    Year_num      = as.integer(Year),
    Unemployment  = as.numeric(Unemployment),
    Inflation     = as.numeric(Inflation),
    Economic_Participation_Rate = as.numeric(Economic_Participation_Rate),
    # ساختار: Industry؛ در صورت تمایل می‌توانی با Services_GDP_Share جایگزین کنی
    Industry_Share_of_GDP       = as.numeric(Industry_Share_of_GDP),
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

# 3) وقفه‌های بیکاری برای پویایی/ابزارها
p_un <- pdata.frame(unok, index = c("Province_Name","Year"), drop.index = FALSE)
unok$Lag_Unemployment <- as.numeric(plm::lag(p_un$Unemployment, 1))
unok$L2_Unemployment  <- as.numeric(plm::lag(p_un$Unemployment, 2))
unok$L3_Unemployment  <- as.numeric(plm::lag(p_un$Unemployment, 3))

# 4) OFE نسبت به 5 سری ملی
ofe_obj_unok <- make_OFE(
  unok, year_var = "Year_num",
  series = c("Interest_Rate","Currency_Price_Growth_Rate",
             "Gasoline_Price_Growth_Rate","Sanctions_Index","Liquidity_Growth_Rate")
)
unok <- attach_OFE(unok, ofe_obj_unok, year_var = "Year_num")
ofe_cols_unok <- grep("^OFE_", names(unok), value = TRUE)

# 5) ساخت W_Inflation (شبکه قیمتی) سال‌به‌سال
unok <- add_W_by_year(unok, Wlist, vars = c("Inflation"), year_var = "Year_num")

# 6) برش برای ابزارهای بی‌NA (2004+) و pdata.frame
unok_cc <- unok %>%
  filter(Year_num >= 2004) %>%
  filter(!is.na(L2_Unemployment) & !is.na(L3_Unemployment))
p_unok_cc <- pdata.frame(unok_cc, index = c("Province_Name","Year"), drop.index = FALSE)

# 7) مشخصه مقاله: Dynamic SLX شبکه‌ای برای OK-Unemployment
rhs_un_ok <- c(
  "Lag_Unemployment",                 # پویایی (درون‌زا)
  "Inflation", "W_Inflation",         # فیلیپس شبکه‌ای
  "Economic_Participation_Rate",      # بازار کار
  "Industry_Share_of_GDP",            # ساختار (در صورت نیاز Services را جایگزین کن)
  "Natural_Disasters",                # شوک محلی
  ofe_cols_unok                       # OFE زمان
)
fmla_un_ok <- as.formula(paste("Unemployment ~", paste(rhs_un_ok, collapse = " + ")))

fit_un_ok <- spgm(
  fmla_un_ok,
  data            = p_unok_cc,
  index           = c("Province_Name","Year"),
  listw           = Wlist,
  model           = "within",      # FE استانی
  lag             = FALSE,         # Wy_it نمی‌آوریم
  spatial.error   = FALSE,
  method          = "g2sls",
  moments         = "fullweights",
  endog           = ~ Lag_Unemployment,                 # درون‌زا
  instruments     = ~ L2_Unemployment + L3_Unemployment +
    Economic_Participation_Rate + Industry_Share_of_GDP + Natural_Disasters,
  lag.instruments = FALSE,
  verbose         = TRUE
)

print(summary(fit_un_ok))
save_model_outputs(fit_un_ok, "UN_OK_main")  # ذخیره در outputs/article/models/