# code/utils.R
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(plm)
  library(spdep)
  library(splm)
  library(lmtest)
  library(sandwich)
  library(readr)
  library(writexl)
})

# ترتیب ثابت استان‌ها (همسو با ماتریس وزن)
province_levels <- c("Ilam","West Azar","Khuzestan","Kurdestan","Kermanshah")

# 1) ساخت Wlist از فایل اکسل وزن‌ها
build_Wlist <- function(wpath = "weights/spatial_weight_matrix_final.xlsx") {
  if (!file.exists(wpath)) stop("Weight file not found: ", wpath)
  W_raw <- read_excel(wpath, col_names = FALSE)
  W <- as.matrix(data.frame(W_raw[2:6, 2:6]))
  storage.mode(W) <- "numeric"
  rownames(W) <- colnames(W) <- province_levels
  if (!all(round(rowSums(W), 6) == 1)) warning("Row sums of W are not 1")
  if (!all(diag(W) == 0)) warning("Diagonal of W is not zero")
  mat2listw(W, style = "W")
}

# 2) خواندن Wlist از RDS (اگر نبود، ساخت و ذخیره)
read_Wlist_safe <- function(rds_path = "weights/Wlist.rds",
                            wpath = "weights/spatial_weight_matrix_final.xlsx") {
  if (file.exists(rds_path)) {
    readRDS(rds_path)
  } else {
    lw <- build_Wlist(wpath)
    dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(lw, rds_path)
    lw
  }
}

# 3) ساخت اثرات زمانی ارتوگونال (OFE) نسبت به سری‌های ملی
make_OFE <- function(df, year_var = "Year_num",
                     series = c("Interest_Rate","Currency_Price_Growth_Rate",
                                "Gasoline_Price_Growth_Rate","Sanctions_Index",
                                "Liquidity_Growth_Rate")) {
  missing <- setdiff(series, names(df))
  if (length(missing)) stop("Missing series in df: ", paste(missing, collapse = ", "))
  Z <- df %>%
    group_by(.data[[year_var]]) %>%
    summarise(across(all_of(series), ~ first(.x)), .groups = "drop") %>%
    arrange(.data[[year_var]])
  yrs <- Z[[year_var]]
  D <- model.matrix(~ factor(yrs) - 1)  # T x T
  colnames(D) <- paste0("Y_", yrs)
  Zm <- as.matrix(Z[, series, drop = FALSE])
  Zc <- scale(Zm, center = TRUE, scale = FALSE)
  PZ <- Zc %*% qr.solve(t(Zc) %*% Zc) %*% t(Zc)
  D_orth <- (diag(nrow(Z)) - PZ) %*% D
  D_orth2 <- D_orth[, -1, drop = FALSE]  # حذف سال پایه
  colnames(D_orth2) <- sub("^Y_", "OFE_", colnames(D_orth2))
  list(OFE = D_orth2, years = yrs)
}

attach_OFE <- function(df, ofe_obj, year_var = "Year_num") {
  match_idx <- match(df[[year_var]], ofe_obj$years)
  for (j in seq_len(ncol(ofe_obj$OFE))) {
    df[[colnames(ofe_obj$OFE)[j]]] <- ofe_obj$OFE[match_idx, j]
  }
  df
}

# 4) ساخت W-لگ سال‌به‌سال برای یک یا چند متغیر
add_W_by_year <- function(df, listw, vars,
                          id_var = "Province_Name", year_var = "Year_num",
                          wprefix = "W_") {
  df[[id_var]] <- factor(as.character(df[[id_var]]), levels = province_levels)
  yrs <- sort(unique(df[[year_var]]))
  for (v in vars) {
    newn <- paste0(wprefix, v)
    df[[newn]] <- NA_real_
    for (tt in yrs) {
      idx <- which(df[[year_var]] == tt)
      ord <- order(df[[id_var]][idx])
      vec <- as.numeric(df[[v]][idx][ord])
      wlag <- lag.listw(listw, vec, zero.policy = TRUE, NAOK = TRUE)
      tmp <- df[[newn]][idx]
      tmp[ord] <- as.numeric(wlag)
      df[[newn]][idx] <- tmp
    }
  }
  df
}

# 5) ذخیره خروجی‌های مدل‌ها (summary + coef + RDS)
save_model_outputs <- function(obj, name, outdir = "outputs/article/models") {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  # خلاصه متنی
  writeLines(capture.output(summary(obj)),
             file.path(outdir, paste0(name, "_summary.txt")))
  # ضرایب، SE و p (تقریب نرمال)
  est <- try(coef(obj), silent = TRUE)
  V   <- try(vcov(obj), silent = TRUE)
  if (!inherits(est, "try-error")) {
    if (!inherits(V, "try-error")) {
      se <- sqrt(diag(V))[names(est)]
      z  <- suppressWarnings(est / se)
      p  <- suppressWarnings(2 * pnorm(abs(z), lower.tail = FALSE))
    } else {
      se <- rep(NA_real_, length(est)); z <- se; p <- se
    }
    df <- data.frame(term = names(est),
                     estimate = as.numeric(est),
                     std.error = as.numeric(se),
                     z.value = as.numeric(z),
                     p.value = as.numeric(p),
                     row.names = NULL)
    readr::write_excel_csv(df, file.path(outdir, paste0(name, "_coef_utf8.csv")), na = "")
    writexl::write_xlsx(df,      file.path(outdir, paste0(name, "_coef.xlsx")))
  }
  # شیء مدل
  saveRDS(obj, file.path(outdir, paste0(name, ".rds")))
}

# 6) پیام آماده بودن
message("utils.R loaded: province_levels, build/read Wlist, make/attach_OFE, add_W_by_year, save_model_outputs")
