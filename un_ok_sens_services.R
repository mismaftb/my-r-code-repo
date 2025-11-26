# code/un_ok_sens_services.R — UN_OK sensitivity: Industry → Services
source("code/utils.R")
Wlist <- read_Wlist_safe("weights/Wlist.rds", "weights/spatial_weight_matrix_final.xlsx")

raw <- readxl::read_excel("data/UN_OK.xlsx")

if (!"Services_GDP_Share" %in% names(raw)) {
  stop("ستون Services_GDP_Share در UN_OK.xlsx پیدا نشد. اگر نام دیگری دارد بگو تا اصلاح کنم.")
}

unok <- raw %>%
  transmute(
    Province_Name = factor(trimws(as.character(Province_Name)), levels = province_levels),
    Year          = as.integer(Year),
    Year_num      = as.integer(Year),
    Unemployment  = as.numeric(Unemployment),
    Inflation     = as.numeric(Inflation),
    Economic_Participation_Rate = as.numeric(Economic_Participation_Rate),
    Services_GDP_Share          = as.numeric(Services_GDP_Share),
    Interest_Rate              = as.numeric(Interest_Rate),
    Currency_Price_Growth_Rate = as.numeric(Currency_Price_Growth_Rate),
    Gasoline_Price_Growth_Rate = as.numeric(Gasoline_Price_Growth_Rate),
    Sanctions_Index            = as.numeric(Sanctions_Index),
    Liquidity_Growth_Rate      = as.numeric(Liquidity_Growth_Rate),
    Natural_Disasters          = as.numeric(Natural_Disasters)
  ) %>% arrange(Province_Name, Year)

p_un <- pdata.frame(unok, index = c("Province_Name","Year"), drop.index = FALSE)
unok$Lag_Unemployment <- as.numeric(plm::lag(p_un$Unemployment, 1))
unok$L2_Unemployment  <- as.numeric(plm::lag(p_un$Unemployment, 2))
unok$L3_Unemployment  <- as.numeric(plm::lag(p_un$Unemployment, 3))

ofe_obj <- make_OFE(
  unok, year_var = "Year_num",
  series = c("Interest_Rate","Currency_Price_Growth_Rate",
             "Gasoline_Price_Growth_Rate","Sanctions_Index","Liquidity_Growth_Rate")
)
unok <- attach_OFE(unok, ofe_obj, year_var = "Year_num")
ofe_cols <- grep("^OFE_", names(unok), value = TRUE)

unok <- add_W_by_year(unok, Wlist, vars = c("Inflation"), year_var = "Year_num")

unok_cc <- unok %>%
  filter(Year_num >= 2004) %>%
  filter(!is.na(L2_Unemployment) & !is.na(L3_Unemployment))
p_unok_cc <- pdata.frame(unok_cc, index = c("Province_Name","Year"), drop.index = FALSE)

rhs <- c(
  "Lag_Unemployment",
  "Inflation","W_Inflation",
  "Economic_Participation_Rate","Services_GDP_Share","Natural_Disasters",
  ofe_cols
)
fmla_un_ok_serv <- as.formula(paste("Unemployment ~", paste(rhs, collapse = " + ")))

fit_un_ok_serv <- spgm(
  fmla_un_ok_serv,
  data            = p_unok_cc,
  index           = c("Province_Name","Year"),
  listw           = Wlist,
  model           = "within",
  lag             = FALSE,
  spatial.error   = FALSE,
  method          = "g2sls",
  moments         = "fullweights",
  endog           = ~ Lag_Unemployment,
  instruments     = ~ L2_Unemployment + L3_Unemployment +
    Economic_Participation_Rate + Services_GDP_Share + Natural_Disasters,
  lag.instruments = FALSE,
  verbose         = TRUE
)

print(summary(fit_un_ok_serv))
save_model_outputs(fit_un_ok_serv, "UN_OK_sens_Services")