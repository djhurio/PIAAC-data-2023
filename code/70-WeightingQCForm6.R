# Weighting QC Forms W-6

# Options
options(max.print = 10e3)
getOption("max.print")

# Reset
rm(list = ls())
gc()


# Functions
variable.format <- function(DT, varname) {
  
  variable <- DT[[varname]]
  
  if (inherits(variable, "POSIXt")) {
    variable <- as.character(variable)
  }
  
  variable_chr <- variable[!is.na(variable)] |> as.character()
  
  if (length(variable_chr)) {
    
    if (is.numeric(variable)) {
      type <- "N"
    } else if (is.character(variable)) {
      type <- "C"
    } else {
      type <- NA_character_
    }
    
    length_tot <- variable_chr |> nchar() |> max()
    
    if (is.numeric(variable) & any(grepl("\\.", variable_chr))) {
      length_dec <- sub("^[0-9]*\\.", "", variable_chr) |> nchar() |> max()
    } else {
      length_dec <- 0
    }
    
    if (length_dec > 0) {
      format <- paste(length_tot, length_dec, sep = ".")
    } else {
      format <- as.character(length_tot)
    }
    
  } else {
    type <- "X"
    format <- "0"
    length_tot <- 0L
    length_dec <- 0L
  }
  
  data.table(
    variable.name = varname,
    type = type,
    format = format,
    length_tot = length_tot,
    length_dec = length_dec
  )
}



# Weighting QC Form W-6 LVA
# https://piaac.ets.org/portal/weighting-qc-form-w-6-lva/
# WeightingQCForm-6_FinalWt_LVA.xlsx

# SDIF - final
dat_sdif <- fread(
  file = "result/CY2_Final_SDIF_LVA.csvy",
  yaml = TRUE
)
dat_sdif[, .N]

# WIF_QCChecks
dat_wif <- haven::read_sas(
  data_file = "data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT()
dat_wif[, .N]

# BQ
dat_bq <- readRDS(file = "data/dat_bq.rds")



grep("VEME", names(dat_wif), value = T)
setnames(dat_wif, "VEMETHODN", "VEMETHOD")
dat_wif[, .N, keyby = .(VEMETHOD)]

# WIF_QCChecks meatdata
dat_wif_meta_tmp <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WIF_QCChecks_metadata.xlsx"
) |> setDT(check.names = T) |> setnames(tolower)
names(dat_wif_meta_tmp)

dat_wif_meta_tmp[, .(variable.name, format, type)]

dat_wif_meta_SPFWT <- cbind(
  data.table(variable.name = paste0("SPFWT", 1:80)),
  dat_wif_meta_tmp[grep("^SPFWT1", variable.name), .SD, .SDcols = -1]
)

dat_wif_meta <- rbindlist(l = list(
  dat_wif_meta_tmp[-grep("^SPFWT1", variable.name)],
  dat_wif_meta_SPFWT
))

rm(dat_wif_meta_tmp, dat_wif_meta_SPFWT)


# Test data
dat_wif_meta[, format := as.character(format)]

dat_wif_meta[, length_tot := sub("\\.[0-9]*$", "", format) |> as.integer()]
dat_wif_meta[, length_dec := ifelse(test = grepl("\\.", format),
                                    yes = sub("^[0-9]*\\.", "", format),
                                    no = "0") |> as.integer()]

dat_wif_meta[, .(variable.name, type, format)]
dat_wif_meta[, .N, keyby = .(type)]
dat_wif_meta[, .N, keyby = .(type, format)]
dat_wif_meta[, .N, keyby = .(type, length_tot, length_dec, format)]

tab_wif_var <- map(
  .x = names(dat_wif),
  .f = \(x) variable.format(DT = dat_wif, varname = x)
) |> rbindlist()
tab_wif_var

tab_wif_var_test <- merge(
  x = dat_wif_meta[, .(variable.name, format, type, length_tot, length_dec)],
  y = tab_wif_var,
  by = "variable.name",
  suffixes = c(".meta", ".data"),
  all = TRUE
)

tab_wif_var_test[is.na(format.meta) | is.na(format.data)]
tab_wif_var_test[type.meta != type.data]
tab_wif_var_test[format.meta != format.data]
tab_wif_var_test[format.meta != format.data][-grep("SPFWT", variable.name)]



# Merge SDIF & WIF
dat_sdif[, .N] == dat_wif[, .N]

dat_pers <- merge(
  x = dat_sdif,
  y = dat_wif,
  all = T
)

# Subselect
dat_pers <- dat_pers[!is.na(SPFWT0)]

# Table 1
dat_pers[, .N, keyby = .(
  WEIGHTFLG == 1,
  DISP_CIBQ == 1 | (DISP_CIBQ == 7 & DISP_DS == 1),
  !is.na(SPFWT0)
)]

tab1_data <- dat_pers[WEIGHTFLG == 1, .(
  CASEID, PERSID,
  HHBWT0, HHUEWT0, HHNRWT0,
  SPBWT0, SPUEWT0, SPNRWT0, SPLNRWT0, SPRWT0 = NA_real_, SPTWT0, SPFWT0
)][order(-SPFWT0)][1:20] |> melt.data.table(id.vars = c("CASEID", "PERSID"))

tab1_test <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-6_FinalWt_LVA.xlsx",
  rows = 20:40,
  cols = col2int(LETTERS[which(LETTERS == "B"):which(LETTERS == "M")]),
  skip_empty_cols = T,
  na.strings = "."
) |> setDT(check.names = T) |>
  setnames(old = 1:2, new = c("CASEID", "PERSID")) |>
  melt.data.table(id.vars = c("CASEID", "PERSID"))

setorder(tab1_data, CASEID, PERSID)
setorder(tab1_test, CASEID, PERSID)

tab1_data[, id := .I]
tab1_test[, id := .I]

tab1 <- merge(
  x = tab1_data,
  y = tab1_test,
  by = c("CASEID", "PERSID", "id")
)

tab1[
  -grep("SPRWT0", variable.x),
  .(CASEID, PERSID, variable.x, value.x, value.y)
]

# SPRWT0 is not available in WIF
if (!isTRUE(tab1[
  -grep("SPRWT0", variable.x),
  all.equal(value.x, value.y, check.attributes = F)
])) stop("Check Table 1")



# Table 2
dat_pers[, .N, keyby = .(GENDER_R)]
dat_pers[, sprintf("%.2f", sum(SPFWT0))]
dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(GENDER_R)]

dat_pers[, .N, keyby = .(AGE_R)]
dat_pers[, sprintf("%.2f", sum(SPFWT0))]
dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(AGE_R)]

dat_pers[, age_group := trunc((AGE_R - 6) / 10)]
dat_pers[, .(.N, Min = min(AGE_R), Max = max(AGE_R)), keyby = .(age_group)]
dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(age_group)]

dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(RAKEDIM1)]
dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(RAKEDIM2)]
dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(RAKEDIM3)]



# Recode b2_q01lv to ISCED
# dat_nrba[, edu_ISCED := NULL]
dat_bq[
  ,
  y_edu := car::recode(
    var = b2_q01lv,
    recodes = "0:3='0-2'; 4:8='3'; 9:16='4-8'; else=NA"
  )
]
dat_bq[, .N, keyby = .(y_edu, b2_q01lv)]


# Country of birth
dat_bq[, .N, keyby = .(a2_q03a)]
dat_bq[, y_natb := factor(
  x = a2_q03a,
  levels = 1:2,
  labels = c("1 Yes", "2 No")
)]
dat_bq[, .N, keyby = .(a2_q03a, y_natb)]


# Economic activity
dat_bq[c2_q07 %in% 1:2,  y_ecact := 1L]
dat_bq[c2_q07 %in% 3,    y_ecact := 2L]
dat_bq[c2_q07 %in% 4:10, y_ecact := 3L]
dat_bq[, y_ecact := factor(
  x = y_ecact,
  levels = 1:3,
  labels = c("1 Employed", "2 Unemployed", "3 Out of the labor force")
)]


dat_bq[, .(persid, y_edu, y_natb, y_ecact)]

dat_pers <- merge(
  x = dat_pers,
  y = dat_bq[, .(persid, y_edu, y_natb, y_ecact)],
  by.x = "PERSID",
  by.y = "persid",
  all.x = TRUE,
  sort = FALSE
)


dat_pers[, .(.N, sum(SPFWT0)), keyby = .(y_edu)]
dat_pers[, .(.N, sum(SPFWT0)), keyby = .(y_natb)]
dat_pers[, .(.N, sum(SPFWT0)), keyby = .(y_ecact)]

grep("^SPFWT[1-9][0-9]?$", names(dat_pers), value = TRUE) |> length()
grep("^y_", names(dat_pers), value = TRUE)

dat_varest <- melt.data.table(
  data = dat_pers,
  id.vars = c("CASEID", "PERSID", "SPFWT0",
              grep("^y_", names(dat_pers), value = TRUE)),
  measure.vars = grep("^SPFWT[1-9][0-9]?$", names(dat_pers), value = TRUE),
  variable.name = "REP",
  value.name = "REPWGT"
) |> melt.data.table(
  id.vars = c("REP", "CASEID", "PERSID", "SPFWT0", "REPWGT"),
)

dat_varest
str(dat_varest)

dat_varest