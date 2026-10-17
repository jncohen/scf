.scf_years <- seq(1989, 2025, 3)

.scf_cpi_urs <- c(
  `1989` = 1898, `1992` = 2112, `1995` = 2261, `1998` = 2400,
  `2001` = 2614, `2004` = 2785, `2007` = 3058, `2010` = 3204,
  `2013` = 3438, `2016` = 3548, `2019` = 3775, `2022` = 4376, `2025` = 4789
)

.scf_cpi_lag <- c(
  `1989` = 1883 / 1805, `1992` = 2099 / 2048, `1995` = 2250 / 2197,
  `1998` = 2392 / 2360, `2001` = 2597 / 2525, `2004` = 2770 / 2698,
  `2007` = 3041 / 2957, `2010` = 3198 / 3147, `2013` = 3420 / 3369,
  `2016` = 3528 / 3483, `2019` = 3758 / 3691, `2022` = 4315 / 3992,
  `2025` = 4750 / 4625
)

.scf_dollar_vars <- c(
  "income", "asset", "networth", "fin", "liq", "cds", "nmmf", "stocks",
  "bond", "retqliq", "savbnd", "cashli", "othma", "othfin", "nfin", "vehic",
  "houses", "oresre", "nnresre", "bus", "othnfin", "debt", "mrthel",
  "resdbt", "othloc", "ccbal", "bnpl", "install", "odebt", "kgtotal",
  "kghouse", "kgore", "kgbus", "kgstmf", "tpay", "mortpay", "conspay",
  "revpay", "tploan", "ploan1", "ploan2", "ploan3", "ploan4", "ploan5",
  "ploan6", "ploan7", "ploan8", "tlloan", "lloan1", "lloan2", "lloan3",
  "lloan4", "lloan5", "lloan6", "lloan7", "lloan8", "lloan9", "lloan10",
  "lloan11", "lloan12", "equity", "deq", "vlease", "vown", "reteq",
  "norminc", "checking", "saving", "mma", "call", "homeeq", "irakh", "peneq",
  "veh_inst", "edn_inst", "oth_inst", "heloc", "nh_mort", "wageinc",
  "bussefarminc", "intdivinc", "kginc", "ssretinc", "transfothinc",
  "rentinc", "nhnfin", "thrift", "currpen", "futpen", "stmutf", "tfbmutf",
  "gbmutf", "obmutf", "comutf", "omutf", "annuit", "trusts", "actbus",
  "nonactbus", "notxbnd", "mortbnd", "govtbnd", "obnd", "outpen", "outmarg",
  "paymort1", "paymort2", "paymort3", "paymorto", "payloc1", "payloc2",
  "payloc3", "payloco", "payhi1", "payhi2", "paylc1", "paylc2", "paylco",
  "payore1", "payore2", "payore3", "payorev", "payveh1", "payveh2",
  "payveh3", "payveh4", "payvehm", "payveo1", "payveo2", "payveom",
  "payedu1", "payedu2", "payedu3", "payedu4", "payedu5", "payedu6",
  "payedu7", "payiln1", "payiln2", "payiln3", "payiln4", "payiln5",
  "payiln6", "payiln7", "paymarg", "payins", "paypen1", "paypen2", "paypen3",
  "paypen4", "paypen5", "paypen6", "farmbus_kg", "mort1", "mort2", "mort3",
  "farmbus", "penacctwd", "mmda", "mmmf", "foodhome", "foodaway", "fooddelv",
  "prepaid", "faequity", "rent", "vacation", "totspendnh"
)

.scf_check_year <- function(year, what = "Survey year") {
  valid <- names(.scf_cpi_urs)
  if (!as.character(year) %in% valid) {
    stop(sprintf("%s %s is not an SCF year. Valid years: %s.",
                 what, year, paste(valid, collapse = ", ")), call. = FALSE)
  }
}

.scf_deflation_factor <- function(survey_year, from_year) {
  .scf_check_year(survey_year)
  .scf_check_year(from_year, "from_year")
  unname(.scf_cpi_urs[as.character(survey_year)] / .scf_cpi_urs[as.character(from_year)])
}

.scf_detect_dollar_year <- function(df, year) {
  y <- as.character(year)
  if (!y %in% names(.scf_cpi_urs) || !all(c("wageinc", "x5702") %in% names(df))) {
    return(NA_integer_)
  }
  ok <- !is.na(df$x5702) & !is.na(df$wageinc) & df$x5702 > 0 & df$wageinc > 0
  if (!any(ok)) return(NA_integer_)
  base <- stats::median(df$wageinc[ok] / df$x5702[ok]) / .scf_cpi_lag[[y]] * .scf_cpi_urs[[y]]
  gap <- abs(.scf_cpi_urs / base - 1)
  if (min(gap) > 0.001) return(NA_integer_)
  as.integer(names(.scf_cpi_urs)[which.min(gap)])
}

.scf_dollar_year <- function(x, what = "these data") {
  dy <- x$dollar_year
  if (is.null(dy) || is.na(dy)) {
    stop(sprintf(paste0(
      "The dollar year of %s is unknown. Reload the data with scf_load(), ",
      "or set it with scf_design(dollar_year = )."), what), call. = FALSE)
  }
  dy
}

.scf_is_raw <- function(v) {
  grepl("^x[0-9]+$", v)
}
