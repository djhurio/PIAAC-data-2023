# SDIF test

# Reset
rm(list = ls())
gc()
source(".Rprofile")

# SDIF load
dat_sdif <- fread("result/CY2_Final_SDIF_LVA.csvy", yaml = TRUE)

# SCF load
dat_scf <- fread("../PIAAC-sample-2022/data/sample_piaac_scf.csv")

# BQ load
dat_bq <- fread("data/BQ/BQ.csv", dec = ",",
                colClasses = list(numeric = "PERSID"))


# CASEID
# OK, 27629 unique values of CASEID although 27630 households released from the main and reserve sample. Please look into the missing record.
dat_sdif[, length(unique(CASEID))] == 27630

# PERSID
# 7826 nonmissing PERSID, although SM report shows 7827 (=7275+276*2), and W1 form also mentioned 7827 persons. Please look into the missing record.
dat_sdif[!is.na(PERSID), length(unique(PERSID))] == 7827


dat_sdif[PERSID == "13611880019", .(PERSID, DISP_SCR, DISP_CIBQ)]


# ID_HH
# Total number of unique ID_PSU ID_SSU ID_HH = 27629. See comment above for CASEID
unique(dat_sdif[, .(ID_PSU, ID_SSU, ID_HH)])
dat_sdif[, unique(.SD), .SDcols = c("ID_PSU", "ID_SSU", "ID_HH")]
dat_sdif[, unique(data.table(ID_PSU, ID_SSU, ID_HH))]
dat_sdif[, unique(data.table(ID_PSU, ID_SSU, ID_HH))][, .N] == 27630


# REGION
# OK. Slightly different counts from the REGION in the SCF. What is the reason for the difference?

unique(dat_sdif[, .(CASEID, REGION)])
dat_scf[, .(CASEID, REGION)]

merge(
  unique(dat_sdif[, .(CASEID, REGION)]),
  dat_scf[, .(CASEID, REGION)],
  by = "CASEID",
  suffixes = c(".SDIF", ".SCF"),
  all = TRUE
)[, .N, keyby = .(REGION.SDIF, REGION.SCF)]

# URBRUR
# OK. Slightly different counts from the URBRUR in the SCF. What is the reason for the difference?

merge(
  unique(dat_sdif[, .(CASEID, URBRUR)]),
  dat_scf[, .(CASEID, URBRUR)],
  by = "CASEID",
  suffixes = c(".SDIF", ".SCF"),
  all = TRUE
)[, .N, keyby = .(URBRUR.SDIF, URBRUR.SCF)]

# PROB_PSU
# PROB_PSU should be provided with full precision (up to 12 decimal places). PROB_PSU HAS 9 DECIMAL PLACES. Please correct it if the value is rounded.

dat_sdif[, .(PROB_PSU)]
dat_sdif[, .(as.character(PROB_PSU))]
dat_sdif[, .N, keyby = .(nchar(PROB_PSU))]
dat_sdif[, .(.N, CASEID = first(CASEID), first(as.character(PROB_PSU))),
         keyby = .(nchar(PROB_PSU))]

tmp <- fread("result/CY2_Final_SDIF_LVA.csv", colClasses = "character")
map_chr(tmp, class) |> table()

tmp[, .(CASEID, PROB_PSU)]
tmp[, .N, keyby = .(nchar(PROB_PSU))]
tmp[, .(.N, CASEID = first(CASEID), first(PROB_PSU)),
         keyby = .(nchar(PROB_PSU))]

# PROB_HH
# PROB_HH should be provided with full precision (up to 12 decimal places). PROB_HH HAS 9 DECIMAL PLACES. Please correct it if the value is rounded.

tmp[, .(CASEID, PROB_HH)]
tmp[, .N, keyby = .(nchar(PROB_HH))]
tmp[, .(.N, CASEID = first(CASEID), first(PROB_HH)),
    keyby = .(nchar(PROB_HH))]

# PROB_PERS
# PROB_PERS should be provided with full precision (up to 12 decimal places). PROB_HH HAS 9 DECIMAL PLACES. Please correct it if the value is rounded.

tmp[, .(CASEID, PROB_PERS)]
tmp[, .N, keyby = .(nchar(PROB_PERS))]
tmp[, .(.N, CASEID = first(CASEID), first(PROB_PERS)),
    keyby = .(nchar(PROB_PERS))]

rm(tmp)

# NUMELG1
# 2 records with DISP_SCR=2 and NUMSEL1=2 but NUMELG1=2. Since 2 people were supposed to be selected only when a household has 4 or more eligible people, please check the value of NUMELG1.

dat_sdif[DISP_SCR == 2 & NUMSEL1 == 2 & NUMELG1 == 2, .N]


# SCQAGERANGE
# 1 case (PERSID: 14168703028) has SCQAGERANGE=39 although the value should be no more than 6. Also this case has both SCQAGE and SCQAGERANGE nonmissing. Please look into it and update SCQAGERANGE.

dat_sdif[, .(.N, min(SCQAGE), max(SCQAGE)), keyby = .(SCQAGERANGE)]
dat_sdif[PERSID == "14168703028", .(SCQAGE, SCQAGERANGE)]

# CI_AGE
# Missing for 30 people. Please impute the missing values, since CI_AGE will be used in the weighting process.

# DISP_CIBQ = 1, or DISP_CIBQ = 7 and DISP_DS = 1

dat_sdif[, .N, keyby = .(DISP_CIBQ, DISP_DS)]
dat_sdif[, .N, keyby = .(WEIGHTFLG)]
dat_sdif[DISP_CIBQ == 1 | (DISP_CIBQ == 7 & DISP_DS == 1), .N,
         keyby = .(DISP_CIBQ, DISP_DS, WEIGHTFLG)]

dat_sdif[, .N, keyby = as.logical(
  DISP_CIBQ == 1 | (DISP_CIBQ == 7 & DISP_DS == 1)
)]

dat_sdif[(WEIGHTFLG), .N]


dat_sdif[, all.equal(
  !is.na(DISP_CIBQ) & (DISP_CIBQ == 1 | (DISP_CIBQ == 7 & DISP_DS == 1)),
  as.logical(WEIGHTFLG)
)]


dat_sdif[DISP_SCR %in% 1:2, .N, keyby = .(CI_AGE, CALCAGE)]
dat_sdif[as.logical(WEIGHTFLG), .N, keyby = .(CI_AGE, CALCAGE)]


# CI_GENDER
# Missing for 4 people.  Please impute the missing values, since CI_GENDER will be used in the weighting process.

dat_sdif[DISP_SCR %in% 1:2, .N, keyby = .(CI_GENDER)]
dat_sdif[DISP_SCR %in% 1:2, .N, keyby = .(CI_GENDER, GENDER)]
dat_sdif[as.logical(WEIGHTFLG), .N, keyby = .(CI_GENDER, GENDER)]


# DISP_SCR
# 7274 with DISP_SCR=1 and 552 records with DISP_SCR=2. Final SM report shows 7275 for DISP_SCR=1. This may be related to the comment for PERSID above.

dat_sdif[DISP_SCR == 1, .N] # 7275
dat_sdif[DISP_SCR == 2, .N] # 552


# V_AGE
# V_AGE=0 (age not confirmed correct by respondent) for 44 BQ respondents. Please look into these cases and make sure the correct persons were interviewed.

dat_sdif[V_AGE == 0, .N]


# V_GENDER
# V_GENDER=0 (gender not confirmed correct by respondent) for 1 BQ respondent (PERSID: 12191581028). Please look into it and make sure the correct person was interviewed.

dat_sdif[V_GENDER == 0, .N]
dat_sdif[PERSID == "12191581028", .(PERSID, V_GENDER)]


# DISP_CIBQ
# (1) one case (PERSID: 12491582019) has QCFLAG=2(validated unacceptably, data deleted and not re-fielded) but DISP_CIBQ=4. You commented in the Record Consistency Checks the record 'is a case of a refusal by the sampled person'. Is the QCFLAG correct? If the QCFLAG is correct, please change DISP_CIBQ to 17.

dat_sdif[PERSID == "12491582019", .(QCFLAG, DISP_CIBQ, WEIGHTFLG)]

# DISP_CIBQ
# (2) one case (PERSID: 12253029028) has DISP_CIBQ=27.  Why is it coded as 27? Could you provide IDs of the record it is a duplicate of? Have you adjusted the probability of the corresponding record? If so, how?

dat_sdif[PERSID == "12253029028", .(QCFLAG, DISP_CIBQ, WEIGHTFLG)]


# ATTMPTTUT
# 43 cases have DISP_TUT=3 (partial complete) but ATTMPTTUT missing. Please look into them to make sure ATTMPTTUT and DISP_TUT are correct.

dat_sdif[DISP_TUT == 3 & is.na(ATTMPTTUT), .N]



# ATTMPTCMP
# 12 cases have DISP_CMP=3 (partial complete) but ATTMPTCMP missing. Please look into them to make sure ATTMPTCMP and DISP_CMP are correct.

dat_sdif[DISP_CMP == 3 & is.na(ATTMPTCMP), .N]


# ATTMPTCBA
# 46 cases have DISP_CBA=3 (partial complete) but ATTMPTCBA missing. Please look into them to make sure ATTMPTCBA and DISP_CBA are correct.

dat_sdif[DISP_CBA == 3 & is.na(ATTMPTCBA), .N]


# RAKEDIM1
# There are 28 discrepancies between RAKEDIM1 and GENDER_R*AGE_R. We will update RAKEDIM1 based on GENDER_R and AGE_R in the weighting process. Please see the updates in 'RAKEDIM1_update' worksheet, and let me know if any of them does not look correct. 

dat_sdif[as.logical(WEIGHTFLG), .N, keyby = .(GENDER, GENDER_R)]
dat_sdif[as.logical(WEIGHTFLG), .N, keyby = .(CALCAGE, AGE_R)]

dat_sdif[as.logical(WEIGHTFLG), .N, keyby = .(GENDER, CI_GENDER)]
dat_sdif[as.logical(WEIGHTFLG), .N, keyby = .(CALCAGE, CI_AGE)]

dat_sdif[as.logical(WEIGHTFLG) & (GENDER != CI_GENDER), .N]
dat_sdif[as.logical(WEIGHTFLG) & (CALCAGE != CI_AGE), .N]

dat_RAKEDIM1_update <- openxlsx::read.xlsx(
  xlsxFile = "feedback/Feedback_on_MS_Final_SDIF_LVA_2023-09-29_v2.xlsx",
  sheet = "RAKEDIM1_update"
) |> setDT()

dat_sdif[, .N, keyby = .(GENDER_R, AGE_R)]

tmp <- merge(
  x = dat_RAKEDIM1_update,
  y = dat_sdif[, .(PERSID, WEIGHTFLG,
                   GENDER, CALCAGE, CI_GENDER, CI_AGE, GENDER_R, AGE_R,
                   RAKEDIM1)],
  suffixes = c("_update", "_original"),
  by = "PERSID",
  all.x = TRUE,
  sort = FALSE
)

tmp[, .N]

tmp[!is.na(GENDER_original), .(GENDER_original, GENDER_update)]
tmp[!is.na(GENDER_original), .(GENDER_original, GENDER_update,
                               CALCAGE_original, CALCAGE_update)]
tmp[!is.na(GENDER_original), .(CI_GENDER, GENDER_update,
                               CI_AGE, CALCAGE_update)]

tmp[is.na(GENDER_original),
    .(WEIGHTFLG, GENDER_original, CI_GENDER, GENDER_update)]

dat_sdif[WEIGHTFLG == 1,
         .(AGE_R_group = paste(min(AGE_R), max(AGE_R), sep = "-")),
         keyby = .(RAKEDIM1, GENDER_R)]

tmp[is.na(GENDER_original),
    .(WEIGHTFLG, GENDER_original, CI_GENDER, CI_AGE)]

# tmp[GENDER == GENDER_R, .N]
# tmp[CALCAGE == AGE_R, .N]
# tmp[GENDER == GENDER_R & CALCAGE == AGE_R, .N]
# tmp[RAKEDIM1.x == RAKEDIM1.y, .N]

tmp <- merge(
  x = dat_RAKEDIM1_update[original.RAKEDIM1 == "missing"],
  y = dat_bq[, .(PERSID, A2_Q03a, A2_Q03bLV, A2_N02LVX)],
  by = "PERSID",
  all.x = TRUE,
  sort = FALSE
)

tmp[, .N]

tmp

rm(tmp)


# PERSVAR1
# Missing for 5.8% of sampled persons. For some of the missing, can you impute them with the education level from BQ? Then for the remaining missing ones, can you impute them using another approach? Since education is highly related with proficiency levels, it would be beneficial to use the education variable for weighting adjustment.

dat_sdif[DISP_SCR %in% 1:2, .N,
         keyby = .(is.na(PERSVAR1))][, P := prop.table(N)][]

dat_bq[, .N, keyby = .(B2_Q01LV)]
dat_bq[B2_Q01LV %in% 0:16, .N]

dat_sdif[, class(PERSID)]
dat_bq[, class(PERSID)]

tmp <- merge(
  x = dat_sdif[DISP_SCR %in% 1:2, .(PERSID, DISP_SCR, PERSVAR1)],
  y = dat_bq[B2_Q01LV %in% 0:16, .(PERSID, B2_Q01LV)],
  by = "PERSID",
  all.x = TRUE
)

tmp[, .N, keyby = .(PERSVAR1, is.na(B2_Q01LV))][, P := prop.table(N)][]

tmp[is.na(PERSVAR1), .N, keyby = .(PERSVAR1, B2_Q01LV)]

rm(tmp)


# IFLG_RAKEDIM3
# 8 cases have IFLG_RAKEDIM3=1, although W1 form table 8.4 mentioned 9 were imputed.

dat_sdif[, .N, keyby = .(IFLG_RAKEDIM3)]


# 13611880019
dat_sdif[PERSID == "13611880019", .(PROB_PERS, PROB_OVERALL_PERS)]
dat_sdif[PERSID == "13611880019", .SD, .SDcols = patterns("^PERSVAR")]
dat_sdif[PERSID == "13611880019", .SD, .SDcols = patterns("^DUVAR")]
dat_sdif[PERSID == "13611880019", .SD, .SDcols = patterns("^IFLG")]
