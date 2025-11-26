# code/in_nk_sens_noW.R — IN_NK sensitivity: drop W × Lag(Inflation)
source("code/utils.R")
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

# داده‌ها
raw <- readxl::read_excel("data/IN_NK.xlsx")
innk <- raw %>%
  transmute(
    Province_Name = factor(trimws(as.character(Province_Name)), levels = province_levels),
    Year          = as.integer(Year),
    Year_num      = as.integer(Year),
    Inflation     = as.numeric(Inflation),
    Out_Pot_Gap   = as.numeric(Out_Pot_Gap),
    Inflation_Expectations_Index = as.numeric(Inflation_Expectations_Index),
    Interest_Rate              = as.numeric(Interest_Rate),
    Currency_Price_Growth_Rate = as.numeric(Currency_Price_Growth_Rate),
    Gasoline_Price_Growth_Rate = as.numeric(Gasoline_Price_Growth_Rate),
    Sanctions_Index            = as.numeric(Sanctions_Index),
    Liquidity_Growth_Rate      = as.numeric(Liquidity_Growth_Rate),
    Natural_Disasters          = as.numeric(Natural_Disasters)
  ) %>% arrange(Province_Name, Year)

# وقفه‌ها و Exp
p_in <- pdata.frame(innk, index = c("Province_Name","Year"), drop.index = FALSE)
innk$Lag_Inflation <- as.numeric(plm::lag(p_in$Inflation, 1))
innk$L2_Inflation  <- as.numeric(plm::lag(p_in$Inflation, 2))
innk$L3_Inflation  <- as.numeric(plm::lag(p_in$Inflation, 3))
innk$Exp_L1        <- as.numeric(plm::lag(p_in$Inflation_Expectations_Index, 1))

# OFE
ofe_obj <- make_OFE(
  innk, year_var = "Year_num",
  series = c("Interest_Rate","Currency_Price_Growth_Rate",
             "Gasoline_Price_Growth_Rate","Sanctions_Index","Liquidity_Growth_Rate")
)
innk <- attach_OFE(innk, ofe_obj, year_var = "Year_num")
ofe_cols <- grep("^OFE_", names(innk), value = TRUE)

# برش نمونه
innk_cc <- innk %>%
  filter(Year_num >= 2004) %>%
  filter(!is.na(L2_Inflation) & !is.na(L3_Inflation))
p_innk_cc <- pdata.frame(innk_cc, index = c("Province_Name","Year"), drop.index = FALSE)

# فرمول حساسیت: بدون W_Lag_Inflation
rhs_noW <- c(
  "Lag_Inflation",
  "Exp_L1",
  "Out_Pot_Gap",
  "Interest_Rate","Currency_Price_Growth_Rate","Gasoline_Price_Growth_Rate","Sanctions_Index",
  "Natural_Disasters",
  ofe_cols
)
fmla_in_nk_noW <- as.formula(paste("Inflation ~", paste(rhs_noW, collapse = " + ")))

fit_in_nk_noW <- spgm(
  fmla_in_nk_noW,
  data            = p_innk_cc,
  index           = c("Province_Name","Year"),
  listw           = Wlist,
  model           = "within",
  lag             = FALSE,
  spatial.error   = FALSE,
  method          = "g2sls",
  moments         = "fullweights",
  endog           = ~ Lag_Inflation,
  instruments     = ~ L2_Inflation + L3_Inflation + Out_Pot_Gap + Natural_Disasters,
  lag.instruments = FALSE,
  verbose         = TRUE
)

print(summary(fit_in_nk_noW))
save_model_outputs(fit_in_nk_noW, "IN_NK_sens_noW")