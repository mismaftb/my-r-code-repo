# code/un_nk.R — New Keynesian Unemployment (Dynamic SLX: y_{t-1} + House/W_House + W_Inflation)
source("code/utils.R")

# 1) وزن فضایی
Wlist <- read_Wlist_safe(
  rds_path = "weights/Wlist.rds",
  wpath    = "weights/spatial_weight_matrix_final.xlsx"
)

# 2) خواندن داده بیکاری NK
raw <- readxl::read_excel("data/UN_NK.xlsx")

nk <- raw %>%
  transmute(
    Province_Name = factor(trimws(as.character(Province_Name)), levels = province_levels),
    Year          = as.integer(Year),
    Year_num      = as.integer(Year),
    # وابسته و کلیدی‌ها
    Unemployment  = as.numeric(Unemployment),
    Inflation     = as.numeric(Inflation),
    House_Cons_Share = as.numeric(House_Cons_Share),
    Economic_Participation_Rate = as.numeric(Economic_Participation_Rate),
    Industry_Share_of_GDP       = as.numeric(Industry_Share_of_GDP),
    # سری‌های ملی برای OFE
    Interest_Rate              = as.numeric(Interest_Rate),
    Currency_Price_Growth_Rate = as.numeric(Currency_Price_Growth_Rate),
    Liquidity_Growth_Rate      = as.numeric(Liquidity_Growth_Rate),
    Gasoline_Price_Growth_Rate = as.numeric(Gasoline_Price_Growth_Rate),
    Sanctions_Index            = as.numeric(Sanctions_Index),
    # شوک محلی
    Natural_Disasters          = as.numeric(Natural_Disasters)
  ) %>%
  arrange(Province_Name, Year)

# 3) وقفه‌های بیکاری (برای ابزارگذاری y_{t-1})
nk <- nk %>%
  group_by(Province_Name) %>%
  arrange(Year_num, .by_group = TRUE) %>%
  mutate(
    Lag_Unemployment = dplyr::lag(Unemployment, 1),
    L2_Unemployment  = dplyr::lag(Unemployment, 2),
    L3_Unemployment  = dplyr::lag(Unemployment, 3)
  ) %>%
  ungroup()

# 4) OFE زمان نسبت به 5 سری ملی
ofe_obj <- make_OFE(
  nk,
  year_var = "Year_num",
  series   = c("Interest_Rate","Currency_Price_Growth_Rate",
               "Gasoline_Price_Growth_Rate","Sanctions_Index","Liquidity_Growth_Rate")
)
nk <- attach_OFE(nk, ofe_obj, year_var = "Year_num")
ofe_cols <- grep("^OFE_", names(nk), value = TRUE)

# 5) ساخت W-متغیرها (شبکه قیمتی و تقاضای خانوار)
nk <- add_W_by_year(nk, Wlist, vars = c("Inflation","House_Cons_Share"), year_var = "Year_num")

# 6) برش نمونه برای ابزارهای بدون NA (2004 به بعد) و بازسازی پنل
nk_cc <- nk %>%
  filter(Year_num >= 2004) %>%
  filter(!is.na(L2_Unemployment) & !is.na(L3_Unemployment))

p_nk_cc <- pdata.frame(nk_cc, index = c("Province_Name","Year"), drop.index = FALSE)

# 7) مشخصه مقاله (Dynamic SLX شبکه‌ای)
rhs <- c(
  "Lag_Unemployment",                # پویایی (درون‌زا)
  "House_Cons_Share","W_House_Cons_Share",  # شبکه تقاضای خصوصی
  "W_Inflation",                     # شبکه قیمتی
  "Economic_Participation_Rate",     # بازار کار
  "Industry_Share_of_GDP",           # ساختار (در صورت نیاز Services را جایگزین کن)
  "Natural_Disasters",               # شوک محلی
  ofe_cols                           # OFE زمان
)
fmla <- as.formula(paste("Unemployment ~", paste(rhs, collapse = " + ")))

fit_un_nk <- spgm(
  fmla,
  data            = p_nk_cc,
  index           = c("Province_Name","Year"),
  listw           = Wlist,
  model           = "within",      # FE استانی
  lag             = FALSE,         # Wy_it نمی‌آوریم
  spatial.error   = FALSE,
  method          = "g2sls",
  moments         = "fullweights",
  endog           = ~ Lag_Unemployment,                        # درون‌زا
  instruments     = ~ L2_Unemployment + L3_Unemployment +      # ابزارها
    House_Cons_Share + Economic_Participation_Rate +
    Industry_Share_of_GDP + Natural_Disasters,
  lag.instruments = FALSE,
  verbose         = TRUE
)

print(summary(fit_un_nk))
save_model_outputs(fit_un_nk, "UN_NK_main")  # outputs/article/models/