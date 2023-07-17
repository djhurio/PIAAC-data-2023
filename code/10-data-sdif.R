# Explore SDIF data

# Reset
rm(list = ls())
gc()
source(".Rprofile")

# SDIF
# dat_sdif_csv <- fread("data/SDIF/SDIF.csv")
# dat_sdif_sav <- read_spss("data/SDIF/SDIF.sav")

# CSV
dat_sdif_fieldw <- fread("data/SDIF/SDIF.csv", dec = ",") |>
  setnames(tolower)

# # SPSS
# dat_sdif_fieldw <- read_spss("data/SDIF/SDIF.sav") |>
#   setDT() |> setnames(tolower)

dat_sdif_sample <- fread("data-sample/sample_piaac_sdif.csv") |>
  setnames(tolower)

anyDuplicated(dat_sdif_sample[, .(caseid)])
anyDuplicated(dat_sdif_fieldw[, .(caseid, persid)])

tmp <- dat_sdif_fieldw[, .N, keyby = .(caseid)]
tmp[, .N, keyby = .(N)]
rm(tmp)

dat_sdif_fieldw[, sum(!is.na(persid))]

# dat_sdif_fieldw[!is.na(persid)] |> View()

tab_sdif_variables <- read.xlsx(
  xlsxFile = "doc/PIAAC_CY2(2022_04)Sampling and Weighting File Layouts.xlsx",
  sheet = "SDIF"
) |> setDT() |> setnames(tolower)

tab_sdif_variables[, format := as.character(format)]

tab_sdif_variables[, length_tot := sub("\\.[0-9]*$", "", format) |> as.integer()]
tab_sdif_variables[, length_dec := ifelse(test = grepl("\\.", format),
                                          yes = sub("^[0-9]*\\.", "", format),
                                          no = "0") |> as.integer()]

tab_sdif_variables[, .(variable.name, type, format)]
tab_sdif_variables[, .N, keyby = .(type)]
tab_sdif_variables[, .N, keyby = .(type, format)]
tab_sdif_variables[, .N, keyby = .(type, length_tot, length_dec, format)]


# paste("#", 1:2, collapse = "\n")
# names(tab_sdif_variables)
variable.test <- function(varname) {
  paste(
    tab_sdif_variables[
      variable.name == varname,
      paste("#", c(variable.name, label, comments), collapse = "\n")
    ],
    glue::glue("dat_sdif_fieldw[, .N, keyby = .({varname})]"),
    "",
    sep = "\n"
  )
}
# variable.test(tab_sdif_variables$variable.name[1])

tmp <- map_chr(.x = tab_sdif_variables$variable.name,
               .f = variable.test)
if (!dir.exists("temp")) dir.create("temp")
fwrite(x = as.data.table(tmp),
       file = "temp/SDIF-variable-test.R",
       quote = FALSE,
       col.names = FALSE)


x <- tab_sdif_variables$variable.name

all.equal(
  names(dat_sdif_fieldw) |> sort(),
  x |> sort()
)

setcolorder(dat_sdif_fieldw, x)
setcolorder(dat_sdif_sample, x[x %in% names(dat_sdif_sample)])
rm(x)


# Check format
# varname <- "endscr"
# varname <- tab_sdif_variables$variable.name[33]
# rm(varname)
# rm(variable)

# class(as.POSIXct(Sys.time()))
# class(as.POSIXlt(Sys.time()))

variable.format <- function(varname) {
  
  variable <- dat_sdif_fieldw[[varname]]
  
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

tab_sdif_var_test <- map(.x = tab_sdif_variables$variable.name,
    .f = variable.format) |> rbindlist()
tab_sdif_var_test

tmp <- merge(tab_sdif_variables, tab_sdif_var_test,
             by = "variable.name", sort = F)
tmp[, .(variable.name, type.x, type.y, format.x, format.y)]

tmp[, .N, keyby = .(type.x, type.y)]
tmp[type.y != "X" & type.y != type.x,
    .(variable.name, type.x, type.y, format.x, format.y)]

tmp[type.y != "X", .N, keyby = .(format.x, format.y)]

tmp[length_tot.y > length_tot.x,
    .(variable.name, type.x, type.y, format.x, format.y)]
dat_sdif_fieldw[, .N, keyby = .(scqage, scqagerange)]
dat_sdif_fieldw[, .N, keyby = .(scqage, ci_age)]

tmp[length_dec.y > length_dec.x,
    .(variable.name, type.x, type.y, format.x, format.y)]



setorder(dat_sdif_fieldw, cntryid, cntry, caseid, persid)
setorder(dat_sdif_sample, cntryid, caseid)


# Mainīgo pārbaude

# cntryid	Country ID
# cntry	Country ID and language sample
dat_sdif_fieldw[, .N, keyby = .(cntryid, cntry)]

# caseid	Household operational ID 
# persid	Person operational identification number (ID) 
dat_sdif_fieldw[, .(caseid, persid)]

dat_sdif_fieldw[is.na(caseid), .N]

all(dat_sdif_fieldw$caseid %in% dat_sdif_sample$caseid)
all(dat_sdif_sample$caseid %in% dat_sdif_fieldw$caseid)

dat_sdif_fieldw[, caseid] |> table() |> table()

dat_sdif_fieldw[!is.na(persid), all(caseid == substr(persid, 1, 8))]

dat_sdif_fieldw[, all(VerifyID(caseid))]
dat_sdif_fieldw[!is.na(persid), all(VerifyID(persid))]

# id_majdes	Sampling ID: Major design stratum ID
dat_sdif_fieldw[, .N, keyby = . (id_majdes)]

# id_psu	Sampling ID: Primary sampling unit (PSU) identification number 
dat_sdif_fieldw[, .N, keyby = . (id_psu)]
dat_sdif_sample[, .N, keyby = . (id_psu)]
all(dat_sdif_fieldw$id_psu %in% dat_sdif_sample$id_psu)
all(dat_sdif_sample$id_psu %in% dat_sdif_fieldw$id_psu)
merge(dat_sdif_sample[, .(caseid, id_psu)],
      dat_sdif_fieldw[, .(caseid, id_psu)],
      by = "caseid")[, all.equal(id_psu.x, id_psu.y)]

# id_ssu	Sampling ID: Second-stage sampling unit (SSU) identification number 
dat_sdif_fieldw[, .N, keyby = . (id_ssu)]

# id_hh	Sampling ID: Household (HH) identification number 
dat_sdif_fieldw[, .(id_psu, id_hh)]
dat_sdif_fieldw[, .N, keyby = .(id_hh)]
dat_sdif_fieldw[, .N, keyby = .(id_psu, id_hh)]
dat_sdif_fieldw[, .N, keyby = .(id_psu, id_hh)][, .N, keyby = .(N)]
dat_sdif_fieldw[, .N, keyby = .(id_psu, id_hh)][N > 1]

all.equal(
  dat_sdif_fieldw[, .N, keyby = .(caseid)][, .N],
  dat_sdif_fieldw[, .N, keyby = .(caseid, id_hh)][, .N]
)



# add_du	Added dwelling unit (DU) 
dat_sdif_fieldw[, .N, keyby = . (add_du)]


# subsamp	Subsample flag 
dat_sdif_fieldw[, .N, keyby = . (subsamp)]
dat_sdif_fieldw[, .(.N, n_pers = sum(!is.na(persid))), keyby = . (subsamp)]
#    subsamp     N n_pers
# 1:       1 18610   5479
# 2:       2  5574   1452
# 3:       3  3724    911


# samptype	Flag for incentive group (FT) or supplemental sample (MS)
dat_sdif_fieldw[, .N, keyby = . (samptype)]


# qcflag_assign	Quality control flag 
dat_sdif_fieldw[, .N, keyby = . (qcflag_assign)][, p := prop.table(N)][]


# smpflg1	Flag to indicate whether any persons should be selected
# from STRATUM 1 within the household
# smpflg2	Flag to indicate whether any persons should be selected
# from STRATUM 2 within the household
# smpflg3	Flag to indicate whether any persons should be selected
# from STRATUM 3 within the household
# ???

dat_sdif_fieldw[, .N, keyby = . (smpflg1, smpflg2, smpflg3)]


# excfrm_prop	Proportion in target population who are excluded from the sampling frame
dat_sdif_fieldw[, .N, keyby = . (excfrm_prop)]


# region
# Region
# Required.
dat_sdif_fieldw[, .N, keyby = .(region)]

# urbrur
# Urban/rural indicator
# Required; 1: Urban, 2: Rural
dat_sdif_fieldw[, .N, keyby = .(urbrur)]

# prob_psu
# First-stage sampling unit probability of selection 
# Required if area PSUs are selected; blank if no PSU selection.
dat_sdif_fieldw[, .N, keyby = .(prob_psu)]

# prob_ssu
# Second-stage sampling unit probability of selection (within prior-stage clusters, if applicable) 
# Required if area SSUs are selected; blank if no SSU selection.
dat_sdif_fieldw[, .N, keyby = .(prob_ssu)]

# prob_hh
# HH probability of selection (within prior-stage clusters, if applicable) 
# Required for all records if household sampling; blank otherwise.
dat_sdif_fieldw[, .N, keyby = .(prob_hh)]

# prob_smpflg1
# Probability that the within-household sampling flag SMPFLG1 was set to 1
# Required for screener countries. Indicates the rate at which the sampling flag was set to 1. Equal to 1 if no stratification for within household selection.
dat_sdif_fieldw[, .N, keyby = .(prob_smpflg1)]

# prob_smpflg2
# Probability that the within-household sampling flag SMPFLG2 was set to 1
# Required for screener countries with non-missing values of SMPFLG2. Indicates the rate at which the sampling flag was set to 1.
dat_sdif_fieldw[, .N, keyby = .(prob_smpflg2)]

# prob_smpflg3
# Probability that the within-household sampling flag SMPFLG3 was set to 1
# Required for screener countries with non-missing values of SMPFLG3. Indicates the rate at which the sampling flag was set to 1.
dat_sdif_fieldw[, .N, keyby = .(prob_smpflg3)]

# strat_psu
# Explicit strata used for stratifying PSUs 
# Required if stratification is used for PSUs; blank otherwise.
dat_sdif_fieldw[, .N, keyby = .(strat_psu)]

# strat_ssu
# Explicit strata used for stratifying SSUs
# Required if stratification is used for SSUs; blank otherwise.
dat_sdif_fieldw[, .N, keyby = .(strat_ssu)]

# strat_hh
# Explicit strata used for stratifying HHs
# Required if stratification is used for households; blank otherwise.
dat_sdif_fieldw[, .N, keyby = .(strat_hh)]

# sort_psu
# Sort order for PSU selection 
# Required if systematic sampling for PSUs; blank otherwise.
dat_sdif_fieldw[, .N, keyby = .(sort_psu)]

# sort_ssu
# Sort order for SSU selection
# Required if systematic sampling for SSUs; blank otherwise.
dat_sdif_fieldw[, .N, keyby = .(sort_ssu)]

# sort_hh
# Sort order for HH selection
# Required if systematic sampling for households; blank otherwise.
dat_sdif_fieldw[, .N, keyby = .(sort_hh)]

# id_oth
# Sampling ID: Other unit identification number 
# Optional; this may be useful when an additional stage of sampling is conducted (e.g., addresses within second-stage units).
dat_sdif_fieldw[, .N, keyby = .(id_oth)]

# prob_oth
# Other stage sampling unit probability of selection, or adjustment for releasing reserve sample 
# Optional; this can be used when an additional sampling stage is conducted or reserve sample is released.
dat_sdif_fieldw[, .N, keyby = .(prob_oth)]

# prob_pers
# Person probability of selection (within HHs, if applicable) 
# Required where PERSID is non-missing. If HHs are selected, this is the within HH selection probability for screener countries (NUMSEL#/ NUMELG#).
dat_sdif_fieldw[, .N, keyby = .(prob_pers)]

# strat_pers
# Explicit strata used for stratifying persons
# Required if stratification is used.
dat_sdif_fieldw[, .N, keyby = .(strat_pers)]

# sort_pers
# Sort order for person selection
# Required if systematic sampling. For iCMS countries, this is filled in with the random number from sample selection.
dat_sdif_fieldw[, .N, keyby = .(sort_pers)]

# intvid
# Interviewer ID
# Non-blank for all records.
dat_sdif_fieldw[, .N, keyby = .(intvid)]

# qcflag
# Quality control validation results
# Required; derived from validation flag and validation results in CMS; &#10;0=not selected,&#10;1=selected, validated acceptably,&#10;2=selected, validated unacceptably, data deleted and not re-fielded&#10;3=selected, validation attempt unsuccessful,&#10;4=selected, validation not attempted.&#10;Note: the mapping between the Validation Result in the iCMA and the QCFLAG is not one-to-one.
dat_sdif_fieldw[, .N, keyby = .(qcflag)]

# qcrec
# Case recording status
# Required;&#10;0=Not applicable&#10;1=Case was observed or recorded&#10;2=Case was supposed to be observed or recorded but was not.
dat_sdif_fieldw[, .N, keyby = .(qcrec)]

# numelg1
# Number of eligible persons in the household from screener (stratum 1)
# Required for HH samples; blank otherwise. If DISP_SCR = 1 or 2, then at least one of NUMELG1-NUMELG3 should be greater than 0.
dat_sdif_fieldw[, .N, keyby = .(numelg1)]

# numsel1
# Number of selected persons in the household from screener (stratum 1)
# Required if NUMELG1 > 0; blank otherwise. 
dat_sdif_fieldw[, .N, keyby = .(numsel1)]

# numelg2
# Number of eligible persons in the household from screener (stratum 2)
# Required for HH samples if 2 or more strata within the HH; blank otherwise. If DISP_SCR = 1 or 2, then at least one of NUMELG1-NUMELG3 should be greater than 0. 
dat_sdif_fieldw[, .N, keyby = .(numelg2)]

# numsel2
# Number of selected persons in the household from screener (stratum 2)
# Required if NUMELG2 > 0; blank otherwise. 
dat_sdif_fieldw[, .N, keyby = .(numsel2)]

# numelg3
# Number of eligible persons in the household from screener (stratum 3)
# Required for HH samples if 3 strata within the HH; blank otherwise. If DISP_SCR = 1 or 2, then at least one of NUMELG1-NUMELG3 should be greater than 0. 
dat_sdif_fieldw[, .N, keyby = .(numelg3)]

# numsel3
# Number of selected persons in the household from screener (stratum 3)
# Required if NUMELG3 > 0; blank otherwise. 
dat_sdif_fieldw[, .N, keyby = .(numsel3)]

# scqage
# Person age (Screener)
# Required for screener countries. Either SCQAGE or SCQAGERANGE should be non-missing for all sampled persons.
dat_sdif_fieldw[, .N, keyby = .(scqage)]

# scqagerange
# Person age category (Screener)
# Required for screener countries if SCQAGE is missing. 
dat_sdif_fieldw[, .N, keyby = .(scqagerange)]

# ci_age
# Person age (Case Initialization)
# Required. Non-missing for all sampled persons. Typically data from registry or screener.
dat_sdif_fieldw[, .N, keyby = .(ci_age)]

# ci_gender
# Person gender (Case Initialization)
# Required. Non-missing for all sampled persons. Typically data from registry or screener.
dat_sdif_fieldw[, .N, keyby = .(ci_gender)]

# disp_scr
# Final disposition code for household for screener
# Required if HH sample; this is blank for one-stage designs.
dat_sdif_fieldw[, .N, keyby = .(disp_scr)]

# v_age
# Result of person age  verification before beginning the BQ
# Required for cases with DISP_CIBQ = 1. 1 = age confirmed correct by respondent; 0 = age not confirmed correct by respondent.
dat_sdif_fieldw[, .N, keyby = .(v_age)]

# v_gender
# Result of person gender verification before beginning the BQ
# Required for cases with DISP_CIBQ = 1. 1 = gender confirmed correct by respondent; 0 = gender not confirmed correct by respondent.
dat_sdif_fieldw[, .N, keyby = .(v_gender)]

# disp_ds
# Final disposition code for the Doorstep Interview
# Required if DISP_CIBQ = 7 and the respondent was routed to the Doorstep Interview.
dat_sdif_fieldw[, .N, keyby = .(disp_ds)]

# disp_cibq
# Final disposition code for person – combining CI and BQ
# Required for registry countries. For screener countries, required if DISP_SCR = 1 or 2.
dat_sdif_fieldw[, .N, keyby = .(disp_cibq)]

# disp_tut
# Final disposition code for person for Tablet Tutorial
# Required if DISP_CIBQ=1. Refer to Figure 5-2 in the Technical Standards and Guidelines (TSG) for the MS assessment design.
dat_sdif_fieldw[, .N, keyby = .(disp_tut)]

# disp_loc
# Final disposition code for person for Locator instrument
# Required if DISP_TUT=1. Refer to Figure 5-2 in the TSG for the MS assessment design. 
dat_sdif_fieldw[, .N, keyby = .(disp_loc)]

# disp_cmp
# Final disposition code for person for Components
# Required if DISP_LOC=1 and RSLTLOC = 1 or 2; should be non-missing for approximately 25% of cases with RSLTLOC = 3. Refer to Figure 5-2 in the TSG for the MS assessment design.
dat_sdif_fieldw[, .N, keyby = .(disp_cmp)]

# disp_cba
# Final disposition code for person for Main task instrument, computer literacy/numeracy/APS
# Required if DISP_CMP=1 and RSLTLOC = 2 or 3 or DISP_CMP is blank and RSLTLOC = 3. Refer to Figure 5-2 in the TSG for the MS assessment design.
dat_sdif_fieldw[, .N, keyby = .(disp_cba)]

# attmpttut
# Flag indicating whether the tablet tutorial was attempted
# Required. &#10;0 or missing = Not attempted&#10;1 = Attempted
dat_sdif_fieldw[, .N, keyby = .(attmpttut)]

# attmptloc
# Flag indicating whether the locator was attempted
# Required. &#10;0 or missing = Not attempted&#10;1 = Attempted
dat_sdif_fieldw[, .N, keyby = .(attmptloc)]

# rsltloc
# Result of locator
# Required.&#10;1 = Fail&#10;2 = Pass Low&#10;3 = Pass High
dat_sdif_fieldw[, .N, keyby = .(rsltloc)]

# attmptcmp
# Flag indicating whether the components were attempted
# Required.&#10;0 or missing = Not attempted&#10;1 = Attempted
dat_sdif_fieldw[, .N, keyby = .(attmptcmp)]

# attmptcba
# Flag indicating whether the CBA was attempted
# Required.&#10;0 or missing = Not attempted&#10;1 = Attempted
dat_sdif_fieldw[, .N, keyby = .(attmptcba)]

# curbobs1
# Curbside observation variable 1 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# Optional.
dat_sdif_fieldw[, .N, keyby = .(curbobs1)]

# curbobs2_01
# Curbside observation option 1 of variable 2 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# Optional.&#10;1 = option 1 selected; 0 = option 1 not selected.
dat_sdif_fieldw[, .N, keyby = .(curbobs2_01)]

# curbobs2_02
# Curbside observation option 2 of variable 2 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# Optional.&#10;1 = option 2 selected; 0 = option 2 not selected.
dat_sdif_fieldw[, .N, keyby = .(curbobs2_02)]

# curbobs2_03
# Curbside observation option 3 of variable 2 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# Optional.&#10;1 = option 3 selected; 0 = option 3 not selected.
dat_sdif_fieldw[, .N, keyby = .(curbobs2_03)]

# curbobs2_04
# Curbside observation option 4 of variable 2 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# Optional.&#10;1 = option 4 selected; 0 = option 4 not selected.
dat_sdif_fieldw[, .N, keyby = .(curbobs2_04)]

# curbobs3
# Curbside observation variable 3 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# Optional.
dat_sdif_fieldw[, .N, keyby = .(curbobs3)]

# zz1a_01
# Observation module: No one else was present during the interview
# Required for all cases with completed BQs.
dat_sdif_fieldw[, .N, keyby = .(zz1a_01)]

# zz1a_02
# Observation module: Someone else was present during the background questionnaire
# Required for all cases with completed BQs.
dat_sdif_fieldw[, .N, keyby = .(zz1a_02)]

# zz1a_03
# Observation module: Someone else was present during the Exercise
# Required for all cases with completed BQs. 
dat_sdif_fieldw[, .N, keyby = .(zz1a_03)]

# zz1b_01
# Observation module: No one assisted the respondent
# Required if ZZ1A_02 = 1 or ZZ1A_03 = 1. 
dat_sdif_fieldw[, .N, keyby = .(zz1b_01)]

# zz1b_02
# Observation module: A translator or interpreter helped the respondent with the background questionnaire
# Required if ZZ1A_02 = 1 or ZZ1A_03 = 1.
dat_sdif_fieldw[, .N, keyby = .(zz1b_02)]

# zz1b_03
# Observation module: Someone assisted the respondent with the Exercise
# Required if ZZ1A_02 = 1 or ZZ1A_03 = 1.
dat_sdif_fieldw[, .N, keyby = .(zz1b_03)]

# zz2
# Observation module: Respondent understood the questions in the background questionnaire
# Required for all cases with completed BQs.
dat_sdif_fieldw[, .N, keyby = .(zz2)]

# zz3
# Observation module: Interviewer assisted the respondent with the Exercise 
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz3)]

# zz4
# Observation module: Respondent asked for assistance in using the features of the tablet
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz4)]

# zz5
# Observation module: Respondent asked for assistance while completing the Exercise
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz5)]

# zz6_01
# Observation module: Air traffic, rail traffic, road traffic including emergency vehicles
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz6_01)]

# zz6_02
# Observation module: Loud noises outside the house
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz6_02)]

# zz6_03
# Observation module: Household appliances in use
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz6_03)]

# zz6_04
# Observation module: Television, radio, game console or music player in use
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz6_04)]

# zz6_05
# Observation module: People talking loudly, crying babies or children, pets
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz6_05)]

# zz6_06
# Observation module: Other background noise during the Exercise
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz6_06)]

# zz6_07
# Observation module: not applicable
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz6_07)]

# zz7
# Observation module: Respondent stopped working on the Exercise
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz7)]

# zz8
# Observation module: Respondent complained Exercise was taking too long
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz8)]

# zz9
# Observation module: Place of the Exercise
# Required for all cases with completed assessments.
dat_sdif_fieldw[, .N, keyby = .(zz9)]

# contacttime1
# # of unsuccessful contact attempts on weekdays before 18:00
# Required for countries using the dashboard. 
dat_sdif_fieldw[, .N, keyby = .(contacttime1)]

# contacttime2
# # of unsuccessful contact attempts on weekdays after 18:00
# Required for countries using the dashboard.
dat_sdif_fieldw[, .N, keyby = .(contacttime2)]

# contacttime3
# # of unsuccessful contact attempts on the weekend
# Required for countries using the dashboard.
dat_sdif_fieldw[, .N, keyby = .(contacttime3)]

# contacttotal
# Total # of unsuccessful contact attempts
# Required for all sampled persons. For screener countries, also required for all households without any sampled persons.
dat_sdif_fieldw[, .N, keyby = .(contacttotal)]

# endscr
# End date and time of screener
# Required for screener countries using the dashboard. Should have the format YYYY-MM-DD HH:MM:SS.
dat_sdif_fieldw[, .N, keyby = .(endscr)]

# prob_overall_hh
# Overall probability of selection of HH
# Required for all records for screener countries; not applicable to registry countries. Product of the probabilities of selection for each stage, excluding the person selection stage.
dat_sdif_fieldw[, .N, keyby = .(prob_overall_hh)]

# prob_overall_pers
# Overall probability of selection of the sampled person
# Required for all sampled persons. Product of the probabilities of selection for each stage.
dat_sdif_fieldw[, .N, keyby = .(prob_overall_pers)]

# theor_hbwt
# Theoretical base weight for selected HH (inverse overall selection probability of HH)
# Required if household sampling; blank otherwise.
dat_sdif_fieldw[, .N, keyby = .(theor_hbwt)]

# theor_pbwt
# Theoretical base weight for selected person (inverse overall selection probability of person – no NR adjustments)
# Required for all sampled persons.
dat_sdif_fieldw[, .N, keyby = .(theor_pbwt)]

# dobyy
# Date of birth – year (BQ)
# Required for DISP_CIBQ = 1 (yyyy). BQ variable A2_Q01a.
dat_sdif_fieldw[, .N, keyby = .(dobyy)]

# dobmm
# Date of birth – month (BQ)
# Required for DISP_CIBQ = 1 (mm). BQ variable A2_Q01b.
dat_sdif_fieldw[, .N, keyby = .(dobmm)]

# calcage
# Person age, derived from BQ 
# Required; derived from DOBYY and DOBMM and date of interview. Imputed from CI_AGE if DOBYY is missing.
dat_sdif_fieldw[, .N, keyby = .(calcage)]

# diagerange
# Person age category (Doorstep Interview)
# Required; non-missing for DISP_DS = 1. Doorstep Interview variable DI_Q03.
dat_sdif_fieldw[, .N, keyby = .(diagerange)]

# age_r
# Person age 
# Required. Resolved age. Equal to CALCAGE for BQ respondents and imputed from CI_AGE and DI_Q03 (age range) for Doorstep Interview respondents.
dat_sdif_fieldw[, .N, keyby = .(age_r)]

# impflgager
# Imputation flag for AGE_R
# Required; 1 = AGE_R imputed; &#10;0 = AGE_R not imputed.
dat_sdif_fieldw[, .N, keyby = .(impflgager)]

# gender
# Person gender (BQ or Doorstep Interview)
# Required, 1=male, 2=female; BQ variable A2_N02 and Doorstep Interview variable DI_Q02. Non-missing for DISP_CIBQ = 1 or DISP_DS = 1.
dat_sdif_fieldw[, .N, keyby = .(gender)]

# gender_r
# Person gender 
# Required. Resolved gender. 
dat_sdif_fieldw[, .N, keyby = .(gender_r)]

# disp_main
# Final derived disposition code for person for Assessment
# Required.
dat_sdif_fieldw[, .N, keyby = .(disp_main)]

# completeflg
# Completed case flag
# Required.&#10;For the main study, defined as:&#10;1: DISP_CIBQ = 1 and ATTMPTTUT = 1 and ATTMPTLOC = 1&#10;2: DISP_DS = 1 &#10;3: DISP_CIBQ = 90 or DISP_MAIN = 90&#10;4: DISP_DS = 90&#10;5: DISP_CIBQ = 1 and DISP_MAIN in (6, 7, 8, 9)&#10;0: Other&#10;This follows Technical Standard 4.3.3 and is used to monitor sample size goals. A limited number of cases with COMPLETEFLG = 2, 4 or 5 will count as completed cases and the limit depends on country’s sample design. All cases with WEIGHTFLG = 1 will receive a final weight.&#10;For the field trial, defined as:&#10;1: Complete based on standard 3.7.3&#10;0: Other.
dat_sdif_fieldw[, .N, keyby = .(completeflg)]

# weightflg
# Weighted case flag
# Required, derived from DISP_CIBQ and DISP_DS. &#10;1: DISP_CIBQ = 1, or DISP_CIBQ = 7 and DISP_DS = 1&#10;0: Other &#10;This flag is used to identify cases in which a non-zero weight is needed.  It includes records that completed the BQ or is a BQ language barrier with completed Doorstep Interview.
dat_sdif_fieldw[, .N, keyby = .(weightflg)]

# excflg
# Exclusion flag
# Required for registry samples only; derived from disposition codes;&#10;1: Inaccessible SP eligible and excluded&#10;2: Inaccessible SP with unknown exclusion status&#10;9: Inaccessible SP known to be ineligible&#10;0: Other.
dat_sdif_fieldw[, .N, keyby = .(excflg)]

# regflg
# Registry situation flag
# Required, for registry samples only&#10;1: Deceased&#10;2: Moved outside country &#10;3: Moved into institution&#10;4: Moved to PIAAC PSU &#10;5: Moved to non-PIAAC PSU &#10;6: Moved to unknown PSU&#10;7: Unknown whereabouts &#10;8: Invalid address, &#10;0: Other.
dat_sdif_fieldw[, .N, keyby = .(regflg)]

# techprob
# Flag for identifying the case as having technical problems during the administration of the interview or assessment
# Required; blank if a case has no technical problems; will be generated automatically for iCMS countries.&#10;1: Zip-file exists but is empty or corrupted&#10;2: Zip-file exists, but one or more files within are corrupt or missing &#10;3:- PDS froze/crashed and re-launch was not possible. The case could not be resumed. &#10;4: The tablet was corrupted or malfunctioned. The case could not be resumed. &#10;5: A CMS issue prevented the case from being started or completed.&#10;6: Other.
dat_sdif_fieldw[, .N, keyby = .(techprob)]

# trimgrps
# Trimming domains
# Required. Identifies the groups for computing the trimming factor. It should be consistent with oversampling domains or strata. Set to 1 if no oversampling.
dat_sdif_fieldw[, .N, keyby = .(trimgrps)]

# rakedim1
# Raking dimension variable 1 – gender by age 
# Required; must match in name and categories with the Weighting International File (WIF) for Benchmark Control Totals. Must be available for all cases with WEIGHTFLG = 1.
dat_sdif_fieldw[, .N, keyby = .(rakedim1)]

# rakedim2
# Raking dimension variable 2
# Required if two or more raking dimension; must match in name and categories with the WIF for Benchmark Control Totals. Must be available for all cases with WEIGHTFLG = 1.
dat_sdif_fieldw[, .N, keyby = .(rakedim2)]

# rakedim3
# Raking dimension variable 3
# Required if three or more raking dimensions; must match in name and categories with the WIF for Benchmark Control Totals. Must be available for all cases with WEIGHTFLG = 1.
dat_sdif_fieldw[, .N, keyby = .(rakedim3)]

# rakedim4
# Raking dimension variable 4
# Required if four or more raking dimensions; must match in name and categories with the WIF for Benchmark Control Totals. Must be available for all cases with WEIGHTFLG = 1.
dat_sdif_fieldw[, .N, keyby = .(rakedim4)]

# rakedim5
# Raking dimension variable 5
# Required if five or more raking dimensions; must match in name and categories with the WIF for Benchmark Control Totals. Must be available for all cases with WEIGHTFLG = 1.
dat_sdif_fieldw[, .N, keyby = .(rakedim5)]

# rakedim6
# Raking dimension variable 6
# Required if six or more raking dimensions; must match in name and categories with the WIF for Benchmark Control Totals. Must be available for all cases with WEIGHTFLG = 1.
dat_sdif_fieldw[, .N, keyby = .(rakedim6)]

# rakedim7
# Raking dimension variable 7
# Required if seven raking dimensions; must match in name and categories with the WIF for Benchmark Control Totals. Must be available for all cases with WEIGHTFLG = 1.
dat_sdif_fieldw[, .N, keyby = .(rakedim7)]

# persvar1
# Person-level variable 1 to be considered for nonresponse bias analysis or for adjustment cells for BQ nonresponse adjustment
# Required; variables other than CI_AGE and CI_GENDER; must be available for both BQ respondents and non-respondents; must have 10 or fewer categories and numbered 0, 1, 2, … 9.
dat_sdif_fieldw[, .N, keyby = .(persvar1)]

# persvar2
# Person-level variable 2 to be considered for nonresponse bias analysis or for adjustment cells for BQ nonresponse adjustment 
# Required if two or more PERSVAR* variable; variables other than CI_AGE and CI_GENDER; must be available for both BQ respondents and non-respondents; must have 10 or fewer categories and numbered 0, 1, 2, … 9.
dat_sdif_fieldw[, .N, keyby = .(persvar2)]

# persvar3
# Person-level variable 3 to be considered for nonresponse bias analysis or for adjustment cells for BQ nonresponse adjustment
# Required if three or more PERSVAR* variables other than CI_AGE and CI_GENDER; must be available for both BQ respondents and non-respondents; must have 10 or fewer categories and numbered 0, 1, 2, … 9. 
dat_sdif_fieldw[, .N, keyby = .(persvar3)]

# persvar4
# Person-level variable 4 to be considered for nonresponse bias analysis or for adjustment cells for BQ nonresponse adjustment
# Required if four or more PERSVAR*variables other than CI_AGE and CI_GENDER; must be available for both BQ respondents and non-respondents; must have 10 or fewer categories and numbered 0, 1, 2, … 9.
dat_sdif_fieldw[, .N, keyby = .(persvar4)]

# persvar5
# Person-level variable 5 to be considered for nonresponse bias analysis or for adjustment cells for BQ nonresponse adjustment
# Required if five PERSVAR* variables other than CI_AGE and CI_GENDER; must be available for both BQ respondents and non-respondents; must have 10 or fewer categories and numbered 0, 1, 2, … 9.
dat_sdif_fieldw[, .N, keyby = .(persvar5)]

# duvar_scrresp1
# Dwelling unit-level screener variable 1 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# Optional for screener countries, non-missing for all HHs with a completed screener. Not applicable to registry countries.
dat_sdif_fieldw[, .N, keyby = .(duvar_scrresp1)]

# duvar_scrresp2
# Dwelling unit-level screener variable 2 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# See DUVAR_SCRRESP1. 
dat_sdif_fieldw[, .N, keyby = .(duvar_scrresp2)]

# duvar_scrresp3
# Dwelling unit-level screener variable 3 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
#  See DUVAR_SCRRESP1.
dat_sdif_fieldw[, .N, keyby = .(duvar_scrresp3)]

# duvar_scrresp4
# Dwelling unit-level screener variable 4 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
#  See DUVAR_SCRRESP1.
dat_sdif_fieldw[, .N, keyby = .(duvar_scrresp4)]

# duvar_scrresp5
# Dwelling unit-level screener variable 5 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
#  See DUVAR_SCRRESP1.
dat_sdif_fieldw[, .N, keyby = .(duvar_scrresp5)]

# duvar_all1
# Dwelling unit-level variable 1 to be considered for nonresponse bias analysis or for nonresponse adjustment cells
# Optional. For registry countries, must be available for all sampled persons. For screener countries, must be available for all sampled DUs. Can come from registry or sampling frame.
dat_sdif_fieldw[, .N, keyby = .(duvar_all1)]

# duvar_all2
# Dwelling unit-level variable 2 to be considered for nonresponse bias analysis or for nonresponse adjustment cells 
# See comment for DUVAR_ALL1. 
dat_sdif_fieldw[, .N, keyby = .(duvar_all2)]

# duvar_all3
# Dwelling unit-level variable 3 to be considered for nonresponse bias analysis or for nonresponse adjustment cells 
# See comment for DUVAR_ALL1.
dat_sdif_fieldw[, .N, keyby = .(duvar_all3)]

# duvar_all4
# Dwelling unit-level variable 4 to be considered for nonresponse bias analysis or for nonresponse adjustment cells 
# See comment for DUVAR_ALL1.
dat_sdif_fieldw[, .N, keyby = .(duvar_all4)]

# duvar_all5
# Dwelling unit-level variable 5 to be considered for nonresponse bias analysis or for nonresponse adjustment cells 
# See comment for DUVAR_ALL1.
dat_sdif_fieldw[, .N, keyby = .(duvar_all5)]

# areavar1
# Area-level variable 1 to be considered for nonresponse bias analysis or for creating nonresponse adjustment cells 
# Optional. For registry countries, non-missing for all sampled persons. For screener countries, non-missing for all sampled dwelling units.
dat_sdif_fieldw[, .N, keyby = .(areavar1)]

# areavar2
# Area-level variable 2 to be considered for nonresponse bias analysis or for creating nonresponse adjustment cells
# See comment for AREAVAR1.
dat_sdif_fieldw[, .N, keyby = .(areavar2)]

# areavar3
# Area-level variable 3 to be considered for nonresponse bias analysis or for creating nonresponse adjustment cells
# See comment for AREAVAR1.
dat_sdif_fieldw[, .N, keyby = .(areavar3)]

# areavar4
# Area-level variable 4 to be considered for nonresponse bias analysis or for creating nonresponse adjustment cells
# See comment for AREAVAR1.
dat_sdif_fieldw[, .N, keyby = .(areavar4)]

# areavar5
# Area-level variable 5 to be considered for nonresponse bias analysis or for creating nonresponse adjustment cells
# See comment for AREAVAR1.
dat_sdif_fieldw[, .N, keyby = .(areavar5)]

# impflgag
# Imputation flag for Age
# Required if any values of CI_AGE were imputed for weighting; 1 = CI_AGE value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(impflgag)]

# impflgge
# Imputation flag for Gender
# Required if any values of CI_GENDER were imputed for weighting; 1 = CI_GENDER value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(impflgge)]

# iflg_persvar1
# Imputation flag for PERSVAR1 
# Required if any values of PERSVAR1 were imputed for weighting; 1 = the value was imputed; 0 = not imputed. 
dat_sdif_fieldw[, .N, keyby = .(iflg_persvar1)]

# iflg_persvar2
# Imputation flag for PERSVAR2
# Required if any values of PERSVAR2 were imputed for weighting; 1 = the value was imputed; 0 = not imputed. 
dat_sdif_fieldw[, .N, keyby = .(iflg_persvar2)]

# iflg_persvar3
# Imputation flag for PERSVAR3 
# Required if any values of PERSVAR3 were imputed for weighting; 1 = the value was imputed; 0 = not imputed. 
dat_sdif_fieldw[, .N, keyby = .(iflg_persvar3)]

# iflg_persvar4
# Imputation flag for PERSVAR4 
# Required if any values of PERSVAR4 were imputed for weighting; 1 = the value was imputed; 0 = not imputed. 
dat_sdif_fieldw[, .N, keyby = .(iflg_persvar4)]

# iflg_persvar5
# Imputation flag for PERSVAR5 
# Required if any values of PERSVAR5 were imputed for weighting; 1 = the value was imputed; 0 = not imputed. 
dat_sdif_fieldw[, .N, keyby = .(iflg_persvar5)]

# iflg_duvar_scrresp1
# Imputation flag for IFLG_DUVAR_SCRRESP1
# Required if any values of DUVAR_SCRRESP1 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_scrresp1)]

# iflg_duvar_scrresp2
# Imputation flag for IFLG_DUVAR_SCRRESP2
# Required if any values of DUVAR_SCRRESP2 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_scrresp2)]

# iflg_duvar_scrresp3
# Imputation flag for IFLG_DUVAR_SCRRESP3
# Required if any values of DUVAR_SCRRESP3 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_scrresp3)]

# iflg_duvar_scrresp4
# Imputation flag for IFLG_DUVAR_SCRRESP4
# Required if any values of DUVAR_SCRRESP14 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_scrresp4)]

# iflg_duvar_scrresp5
# Imputation flag for IFLG_DUVAR_SCRRESP5
# Required if any values of DUVAR_SCRRESP5 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_scrresp5)]

# iflg_duvar_all1
# Imputation flag for DUVAR_ALL1
# Required if any values of DUVAR_ALL1 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_all1)]

# iflg_duvar_all2
# Imputation flag for DUVAR_ALL2
# Required if any values of DUVAR_ALL2 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_all2)]

# iflg_duvar_all3
# Imputation flag for DUVAR_ALL3
# Required if any values of DUVAR_ALL3 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_all3)]

# iflg_duvar_all4
# Imputation flag for DUVAR_ALL4
# Required if any values of DUVAR_ALL4 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_all4)]

# iflg_duvar_all5
# Imputation flag for DUVAR_ALL5
# Required if any values of DUVAR_ALL5 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_duvar_all5)]

# iflg_curbobs1
# Imputation flag for CURBOBS1
# Required if any values of CURBOBS1 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_curbobs1)]

# iflg_curbobs2
# Imputation flag for CURBOBS2_01 to CURBOBS2_04
# Required if any values of CURBOBS2_01 to CURBOBS2_04 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_curbobs2)]

# iflg_curbobs3
# Imputation flag for CURBOBS3
# Required if any values of CURBOBS3 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_curbobs3)]

# iflg_areavar1
# Imputation flag for AREAVAR1 
# Required if any values of AREAVAR1 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_areavar1)]

# iflg_areavar2
# Imputation flag for AREAVAR2
# Required if any values of AREAVAR2 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_areavar2)]

# iflg_areavar3
# Imputation flag for AREAVAR3 
# Required if any values of AREAVAR3 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_areavar3)]

# iflg_areavar4
# Imputation flag for AREAVAR4 
# Required if any values of AREAVAR4 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_areavar4)]

# iflg_areavar5
# Imputation flag for AREAVAR5 
# Required if any values of AREAVAR5 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_areavar5)]

# iflg_rakedim1
# Imputation flag for RAKEDIM1
# Required if any values of RAKEDIM1 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_rakedim1)]

# iflg_rakedim2
# Imputation flag for RAKEDIM2
# Required if any values of RAKEDIM2 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_rakedim2)]

# iflg_rakedim3
# Imputation flag for RAKEDIM3
# Required if any values of RAKEDIM3 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_rakedim3)]

# iflg_rakedim4
# Imputation flag for RAKEDIM4
# Required if any values of RAKEDIM4 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_rakedim4)]

# iflg_rakedim5
# Imputation flag for RAKEDIM5
# Required if any values of RAKEDIM5 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_rakedim5)]

# iflg_rakedim6
# Imputation flag for RAKEDIM6
# Required if any values of RAKEDIM6 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_rakedim6)]

# iflg_rakedim7
# Imputation flag for RAKEDIM7
# Required if any values of RAKEDIM7 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif_fieldw[, .N, keyby = .(iflg_rakedim7)]

