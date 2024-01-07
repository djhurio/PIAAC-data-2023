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
dat_sdif <- haven::read_sas(
  data_file = "data-weights/SAS/SAS7BDAT/psdlvams.sas7bdat"
) |> setDT()

# WIF
dat_wif <- haven::read_sas(
  data_file = "data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT()

# SPRWT0
dat_sprwt <- haven::read_sas(
  data_file = "data-weights/sprwt.sas7bdat"
) |> setDT()

dat_wif <- merge(
  x = dat_wif,
  y = dat_sprwt,
  by = "PERSID",
  all.x = TRUE
)
rm(dat_sprwt)

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
  SPBWT0, SPUEWT0, SPNRWT0, SPLNRWT0, SPRWT0, SPTWT0, SPFWT0
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

tab1[, .(CASEID, PERSID, variable.x, value.x, value.y)]

# SPRWT0 is not available in WIF
if (!isTRUE(tab1[, all.equal(value.x, value.y, check.attributes = F)])) {
  stop("Check Table 1")
}


# Table 2
dat_pers[, .N, keyby = .(GENDER_R)]
dat_pers[, sprintf("%.2f", sum(SPFWT0))]
dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(GENDER_R)]

dat_pers[, y_gender := factor(
  x = GENDER_R,
  levels = 1:2,
  labels = c("1 Male", "2 Female")
)]

dat_pers[, .N, keyby = .(AGE_R)]
dat_pers[, sprintf("%.2f", sum(SPFWT0))]
dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(AGE_R)]

dat_pers[AGE_R %in% 16:65, age_group := trunc((AGE_R - 6) / 10)]
dat_pers[AGE_R > 65, age_group := 5]
dat_pers[, .(.N, Min = min(AGE_R), Max = max(AGE_R)), keyby = .(age_group)]
dat_pers[, sprintf("%.2f", sum(SPFWT0)), keyby = .(age_group)]

dat_pers[, y_age := factor(
  x = age_group,
  levels = 1:5,
  labels = c("16-25", "26-35", "36-45", "46-55", "56-65")
)]

dat_pers[, .N, keyby = .(y_age)]


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
dat_bq[, .N, keyby = .(y_edu)]


# Country of birth
dat_bq[, .N, keyby = .(a2_q03a)]
dat_bq[, y_natb := factor(
  x = a2_q03a,
  levels = 1:2,
  labels = c("1 Yes", "2 No")
)]
dat_bq[, .N, keyby = .(y_natb, a2_q03a)]
dat_bq[, .N, keyby = .(y_natb)]


# Economic activity
dat_bq[c2_q07 %in% 1:2,  y_ecact := 1L]
dat_bq[c2_q07 %in% 3,    y_ecact := 2L]
dat_bq[c2_q07 %in% 4:10, y_ecact := 3L]
dat_bq[, y_ecact := factor(
  x = y_ecact,
  levels = 1:3,
  labels = c("1 Employed", "2 Unemployed", "3 Out of the labor force")
)]
dat_bq[, .N, keyby = .(y_ecact)]



# Merge BQ to data
dat_bq[, .(persid, y_edu, y_natb, y_ecact)]

dat_pers <- merge(
  x = dat_pers,
  y = dat_bq[, .(PERSID = persid, y_edu, y_natb, y_ecact)],
  by = "PERSID",
  all.x = TRUE
)


dat_pers[, .(.N, sum(SPFWT0)), keyby = .(y_edu)]
dat_pers[, .(.N, sum(SPFWT0)), keyby = .(y_natb)]
dat_pers[, .(.N, sum(SPFWT0)), keyby = .(y_ecact)]

dat_pers[, y_rakedim1 := factor(
  x = RAKEDIM1,
  levels = sort(unique(RAKEDIM1)),
  labels = sprintf("%02d", unique(sort(RAKEDIM1)))
)]
dat_pers[, y_rakedim2 := factor(
  x = RAKEDIM2,
  levels = sort(unique(RAKEDIM2)),
  labels = sprintf("%02d", sort(unique(RAKEDIM2)))
)]
dat_pers[, y_rakedim3 := factor(
  x = RAKEDIM3,
  levels = sort(unique(RAKEDIM3)),
  labels = sprintf("%02d", sort(unique(RAKEDIM3)))
)]




dat_pers[, sprintf("%.2f", round(sum(SPFWT0))), keyby = .(y_gender)]
dat_pers[, sprintf("%.2f", round(sum(SPFWT0))), keyby = .(y_age)]
dat_pers[, sprintf("%.2f", round(sum(SPFWT0))), keyby = .(y_rakedim1)]
dat_pers[, sprintf("%.2f", round(sum(SPFWT0))), keyby = .(y_rakedim2)]
dat_pers[, sprintf("%.2f", round(sum(SPFWT0))), keyby = .(y_rakedim3)]



grep("^SPFWT[1-9][0-9]?$", names(dat_pers), value = TRUE) |> length()

# g <- grep("^SPFWT[1-9][0-9]?$", names(dat_pers)) |> length()
# k <- 0.3 # from WeightingQCForm-2_BaseWeights_Replicates_LVA
# 1 / (g * (1 - k) ^ 2)

# y <- "y_gender"
# dt <- copy(dat_pers)

est_var <- function(y, dt = dat_pers, k = 0.3) {

  # Full sample estimates
  dat_var_full <- dt[
    ,
    .(samp_size = .N,
      weight_freq = as.integer(round(sum(SPFWT0)))),
    keyby = c(y)
  ]
  
  dat_var_repl <- melt.data.table(
    data = dt,
    id.vars = c("CASEID", "PERSID", y),
    measure.vars = grep("^SPFWT[1-9][0-9]?$", names(dat_pers), value = TRUE),
    variable.name = "REPLICATE",
    value.name = "REPWGT"
  )[
    ,
    .(samp_size_rep = .N,
      weight_freq_rep = as.integer(round(sum(REPWGT)))),
    keyby = c("REPLICATE", y)
  ]

  dat_var <- merge(
    x = dat_var_repl,
    y = dat_var_full,
    by = y,
    all = TRUE
  )
  
  g <- dat_var[, length(unique(REPLICATE))]
  
  dat_var <- dat_var[
    ,
    .(se = sqrt(sum((weight_freq_rep - weight_freq) ^ 2) / g / (1 - k) ^ 2)),
    keyby = c(y)
  ]
  
  dat_var_res <- merge(
    x = dat_var_full,
    y = dat_var,
    all = TRUE
  )
  
  dat_var_res[, variable := y]
  dat_var_res[, cv := se / weight_freq * 100]
  setnames(dat_var_res, y, "value")
  setcolorder(dat_var_res, c("variable", "value"))
  
  return(dat_var_res[])
}

y_variables <- grep("^y_", names(dat_pers), value = TRUE)
y_variables

tab_w6_3 <- map_dfr(.x = y_variables, .f = est_var)
openxlsx2::write_xlsx(
  x = tab_w6_3,
  file = "WeightingQCForms/tab_w6_3.xlsx"
)

dat_pers[, .(.N, WGT_SUM = sprintf("%.2f", sum(SPFWT0)))]
dat_pers[, .(.N, WGT_SUM = sprintf("%.2f", sum(SPFWT0))), keyby = .(DISP_CIBQ)]


# Weighting International File (WIF) for QC LVA ####
# https://piaac.ets.org/portal/weighting-international-file-wif-for-qc-lva/

# Look at the characteristics of persons with large weights.
# Analysts should be aware of the potential influence that cases with
# large weights can have on their analyses, especially when extreme weights are
# associated with extreme data points.
# 
# Some trimming was performed, but it was kept at a minimum as it can
# introduce bias into the estimates. You may also want to look at subgroups
# that may be of interest in analysis to check for any outliers within the subgroup

dat_pers[
  , .(CASEID, PERSID, SPFWT0, GENDER_R, AGE_R, REGION, y_edu, y_ecact)
][order(-SPFWT0)][1:20]


# As a check on the final weights, the QC forms should include estimates
# of key BQ variables, but please note that the estimates
# for education attainment, native born and employment status are
# left blank in Table 3 of W-6 since we do not have access to the BQ data.
# 
# We encourage you to use your BQ data and the final weights (SPFWT0)
# on the QC WIF to compute estimates for the three variables and return
# W-6 form to us after you complete Table 3.
# 
# We also recommend that you use the final weights to produce
# other estimates of interest and check them for reasonableness.

# Jāsaprot, ko nozīmē papildus novērtējumi.
# IZM (pasūtītāju) varētu interesēt reģionālais, valodu un vecuma grupu griezums.
# Redzot, kāds ir profesiju kodēšanas rezultāts, neredzu, ka tur varētu sanākt
# arī profesiju grupu griezums (mums bija jākodē ar 4 ciparu precizitāti).
# Jāpiebilst, ka mēs arī esam ieguvuši krievvalodīgos mazāk nekā plānots.
# Pavisam maz ir krievvalodīgo jauniešu.

est_var("REGION")

# Kāda ir valoda, kuru Jūs pirmo reizi apguvāt bērnībā, mājās, UN JOPROJĀM SAPROTAT?
dat_bq[, .N, keyby = .(a2_q04a1lv)]

# Kādā valodā Jūs visbiežāk runājat mājās?
dat_bq[, .N, keyby = .(a2_q04blv)]

dat_pers <- merge(
  x = dat_pers,
  y = dat_bq[, .(PERSID = persid, a2_q04a1lv, a2_q04blv)],
  by = "PERSID",
  all.x = TRUE
)

dat_pers[, region := factor(x = REGION, levels = 1:6, labels = c(
  "Rīgas statistiskais reģions",
  "Pierīgas statistiskais reģions",
  "Vidzemes statistiskais reģions",
  "Kurzemes statistiskais reģions",
  "Zemgales statistiskais reģions",
  "Latgales statistiskais reģions"
))]

dat_pers[, valoda_pirma_a2_q04a1lv := factor(
  x = a2_q04a1lv,
  levels = 1:9,
  labels = c(
    "Latviešu",
    "Krievu",
    "Ukraiņu",
    "Baltkrievu",
    "Poļu",
    "Angļu",
    "Igauņu",
    "Vācu",
    "Cita valoda"
  )
)]

dat_pers[, valoda_majas_a2_q04blv := factor(
  x = a2_q04blv,
  levels = 1:7,
  labels = c(
    "Latviešu",
    "Krievu",
    "Ukraiņu",
    "Baltkrievu",
    "Poļu",
    "Angļu",
    "Cita valoda"
  )
)]

dat_pers[, .N, keyby = .(region)]
dat_pers[, .N, keyby = .(valoda_pirma_a2_q04a1lv)]
dat_pers[, .N, keyby = .(valoda_majas_a2_q04blv)]

tab_est <- map_dfr(
  .x = c(
    "region",
    "valoda_pirma_a2_q04a1lv",
    "valoda_majas_a2_q04blv"
  ),
  .f = est_var
)

tab_est <- tab_est[!is.na(value) & !value %in% 97:98]
tab_est

openxlsx2::write_xlsx(
  x = tab_est,
  file = "WeightingQCForms/tab_WIF-estimates_extra.xlsx",
  as_table = TRUE
)
