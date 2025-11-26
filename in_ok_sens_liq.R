# code/in_ok_sens_liq.R — IN_OK sensitivity: Interest → Liquidity
source("code/utils.R")
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

# آماده‌سازی داده‌ها همانند in_ok.R (بدون تکرار غیرضروری)
raw <- readxl::read_excel("data/IN_OK.xlsx")

inok <- raw %>%
  transmute(
    Province_Name = factor(trimws(as.character(Province_Name)), levels = province_levels),
    Year          = as.integer(Year),
    Year_num      = as.integer(Year),
    Inflation     = as.numeric(Inflation),
    Out_Pot_Gap   = as.numeric(Out_Pot_Gap),
    Interest_Rate = as.numeric(Interest_Rate),
    Currency_Price_Growth_Rate  = as.numeric(Currency_Price_Growth_Rate),
    Gasoline_Price_Growth_Rate  = as.numeric(Gasoline_Price_Growth_Rate),
    Sanctions_Index             = as.numeric(Sanctions_Index),
    Natural_Disasters           = as.numeric(Natural_Disasters),
    Liquidity_Growth_Rate       = as.numeric(Liquidity_Growth_Rate)
  ) %>%
  arrange(Province_Name, Year)

p_inok <- pdata.frame(inok, index = c("Province_Name","Year"), drop.index = FALSE)
inok$Lag_Inflation <- as.numeric(plm::lag(p_inok$Inflation, 1))
inok$L2_Inflation  <- as.numeric(plm::lag(p_inok$Inflation, 2))
inok$L3_Inflation  <- as.numeric(plm::lag(p_inok$Inflation, 3))

ofe_obj <- make_OFE(
  inok, year_var = "Year_num",
  series = c("Interest_Rate","Currency_Price_Growth_Rate","Gasoline_Price_Growth_Rate",
             "Sanctions_Index","Liquidity_Growth_Rate")
)
inok <- attach_OFE(inok, ofe_obj, year_var = "Year_num")
ofe_cols <- grep("^OFE_", names(inok), value = TRUE)

inok <- add_W_by_year(inok, Wlist, vars = c("Lag_Inflation","L2_Inflation","L3_Inflation"), year_var = "Year_num")

inok_cc <- inok %>%
  filter(Year_num >= 2004) %>%
  filter(!is.na(L2_Inflation) & !is.na(L3_Inflation))
p_inok_cc <- pdata.frame(inok_cc, index = c("Province_Name","Year"), drop.index = FALSE)

# فرمول حساسیت: Liquidity به‌جای Interest
rhs_liq <- c(
  "Lag_Inflation","W_Lag_Inflation",
  "Out_Pot_Gap",
  "Liquidity_Growth_Rate",
  "Currency_Price_Growth_Rate","Gasoline_Price_Growth_Rate","Sanctions_Index",
  "Natural_Disasters",
  ofe_cols
)
fmla_in_ok_liq <- as.formula(paste("Inflation ~", paste(rhs_liq, collapse = " + ")))

fit_in_ok_liq <- spgm(
  fmla_in_ok_liq,
  data            = p_inok_cc,
  index           = c("Province_Name","Year"),
  listw           = Wlist,
  model           = "within",
  lag             = FALSE,
  spatial.error   = FALSE,
  method          = "g2sls",
  moments         = "fullweights",
  endog           = ~ Lag_Inflation + W_Lag_Inflation,
  instruments     = ~ L2_Inflation + L3_Inflation + W_L2_Inflation + W_L3_Inflation +
    Out_Pot_Gap + Natural_Disasters,
  lag.instruments = FALSE,
  verbose         = TRUE
)

print(summary(fit_in_ok_liq))
save_model_outputs(fit_in_ok_liq, "IN_OK_sens_Liq")