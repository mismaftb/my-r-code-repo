# code/un_nk_sens_no_winfl.R — UN_NK sensitivity: drop W_Inflation
source("code/utils.R")
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

# 1) داده‌ها
raw <- readxl::read_excel("data/UN_NK.xlsx")
nk <- raw %>%
  transmute(
    Province_Name = factor(trimws(as.character(Province_Name)), levels = province_levels),
    Year          = as.integer(Year),
    Year_num      = as.integer(Year),
    Unemployment  = as.numeric(Unemployment),
    Inflation     = as.numeric(Inflation),
    House_Cons_Share            = as.numeric(House_Cons_Share),
    Economic_Participation_Rate = as.numeric(Economic_Participation_Rate),
    Industry_Share_of_GDP       = as.numeric(Industry_Share_of_GDP),
    Interest_Rate              = as.numeric(Interest_Rate),
    Currency_Price_Growth_Rate = as.numeric(Currency_Price_Growth_Rate),
    Liquidity_Growth_Rate      = as.numeric(Liquidity_Growth_Rate),
    Gasoline_Price_Growth_Rate = as.numeric(Gasoline_Price_Growth_Rate),
    Sanctions_Index            = as.numeric(Sanctions_Index),
    Natural_Disasters          = as.numeric(Natural_Disasters)
  ) %>% arrange(Province_Name, Year)

# 2) وقفه‌ها
p_nk <- pdata.frame(nk, index = c("Province_Name","Year"), drop.index = FALSE)
nk$Lag_Unemployment <- as.numeric(plm::lag(p_nk$Unemployment, 1))
nk$L2_Unemployment  <- as.numeric(plm::lag(p_nk$Unemployment, 2))
nk$L3_Unemployment  <- as.numeric(plm::lag(p_nk$Unemployment, 3))

# 3) OFE
ofe_obj <- make_OFE(
  nk, year_var = "Year_num",
  series = c("Interest_Rate","Currency_Price_Growth_Rate",
             "Gasoline_Price_Growth_Rate","Sanctions_Index","Liquidity_Growth_Rate")
)
nk <- attach_OFE(nk, ofe_obj, year_var = "Year_num")
ofe_cols <- grep("^OFE_", names(nk), value = TRUE)

# 4) فقط شبکه تقاضای خانوار (بدون ساخت W_Inflation)
nk <- add_W_by_year(nk, Wlist, vars = c("House_Cons_Share"), year_var = "Year_num")

# 5) برش نمونه
nk_cc <- nk %>%
  filter(Year_num >= 2004) %>%
  filter(!is.na(L2_Unemployment) & !is.na(L3_Unemployment))
p_nk_cc <- pdata.frame(nk_cc, index = c("Province_Name","Year"), drop.index = FALSE)

# 6) فرمول حساسیت: حذف W_Inflation
rhs <- c(
  "Lag_Unemployment",
  "House_Cons_Share","W_House_Cons_Share",
  "Economic_Participation_Rate","Industry_Share_of_GDP","Natural_Disasters",
  ofe_cols
)
fmla_un_nk_noWINF <- as.formula(paste("Unemployment ~", paste(rhs, collapse = " + ")))

fit_un_nk_noWINF <- spgm(
  fmla_un_nk_noWINF,
  data            = p_nk_cc,
  index           = c("Province_Name","Year"),
  listw           = Wlist,
  model           = "within",
  lag             = FALSE,
  spatial.error   = FALSE,
  method          = "g2sls",
  moments         = "fullweights",
  endog           = ~ Lag_Unemployment,
  instruments     = ~ L2_Unemployment + L3_Unemployment +
    House_Cons_Share + Economic_Participation_Rate +
    Industry_Share_of_GDP + Natural_Disasters,
  lag.instruments = FALSE,
  verbose         = TRUE
)

print(summary(fit_un_nk_noWINF))
save_model_outputs(fit_un_nk_noWINF, "UN_NK_sens_no_WINF")