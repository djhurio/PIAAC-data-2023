# Combine and make SDIF

# Reset
rm(list = ls())
gc()

# SDIF
dat_sdif_fieldw <- readRDS(file = "data/dat_sdif_fieldw.rds")

dat_sdif_fieldw[, class(caseid)]
dat_sdif_fieldw[, class(persid)]

dat_raking <- readRDS(file = "data/dat_raking_sel.rds")
dat_scr_case <- readRDS(file = "data/dat_scr_case.rds")
dat_scr_pers <- readRDS(file = "data/dat_scr_pers.rds")

tab_sdif_variables <- read.xlsx(
  xlsxFile = "doc/PIAAC_CY2(2022_04)Sampling and Weighting File Layouts.xlsx",
  sheet = "SDIF"
) |> setDT() |> setnames(tolower)

dat_sdif_sample <- fread(
  file = "data2-sample/sample_piaac_sdif.csvy",
  yaml = TRUE
) |> setnames(tolower)

dat_sdif_sample[, class(caseid)]
dat_sdif_sample[, caseid := as.numeric(caseid)]
dat_sdif_sample[, class(caseid)]

# SUBSAMP is recalculated to values 1, 2, 3
dat_sdif_sample[, .N, keyby = .(subsamp)]
dat_sdif_sample[, subsamp := NULL]

# varnames to delete
x <- setdiff(unique(c(names(dat_sdif_sample), names(dat_raking),
                      names(dat_scr_case), names(dat_scr_pers))),
             c("caseid", "persid"))
dat_sdif_fieldw[, c(x) := NULL]
rm(x)

dat_sdif <- merge(dat_sdif_sample, dat_sdif_fieldw,
                  by = "caseid", all.x = TRUE)
dat_sdif <- merge(dat_sdif, dat_raking,
                  by = "persid", all.x = TRUE)
dat_sdif <- merge(dat_sdif, dat_scr_case,
                  by = "caseid", all.x = TRUE)
dat_sdif <- merge(dat_sdif, dat_scr_pers,
                  by = "persid", all.x = TRUE)

setcolorder(dat_sdif, tab_sdif_variables$variable.name)
setkey(dat_sdif, caseid, persid)


# Extra variables

# PROB_PERS
# Person probability of selection (within HHs, if applicable) 

dat_sdif[, .N, keyby = .(numelg1, numsel1)]
dat_sdif[, .N, keyby = .(numelg2, numsel2)]
dat_sdif[, .N, keyby = .(numelg3, numsel3)]

# Do not recalculate PROB_PERS!
dat_sdif[, .N, keyby = .(prob_pers)]
dat_sdif[, .N, keyby = .(numelg1, numsel1, prob_pers)]
dat_sdif[weightflg == 1, .N, keyby = .(numelg1, numsel1, prob_pers)]


# PROB_OVERALL_PERS
# Do not calculate! Will be calculated by the consorcium.
# Overall probability of selection of the sampled person
# dat_sdif[, prob_overall_pers := round(prob_overall_hh * prob_pers, 12)]
# dat_sdif[weightflg == 1, summary(prob_overall_pers)]
# dat_sdif[weightflg == 1, sum(1 / prob_overall_pers)]

# THEOR_PBWT
# Do not calculate! Will be calculated by the consorcium.
# Theoretical base weight for selected person
# (inverse overall selection probability of person – no NR adjustments)
# dat_sdif[, theor_pbwt := round(1 / prob_overall_pers, 6)]
# dat_sdif[, sum(theor_pbwt, na.rm = T)]
# dat_sdif[, sum(theor_pbwt, na.rm = T), keyby = .(weightflg)]


# Save
setnames(dat_sdif, toupper)

fwrite(
  x = dat_sdif, file = "result/CY2_Final_SDIF_LVA.csv", sep = ";", yaml = F
)
fwrite(
  x = dat_sdif, file = "result/CY2_Final_SDIF_LVA.csvy", sep = ";", yaml = T
)
write.xlsx(
  x = dat_sdif, file = "result/CY2_Final_SDIF_LVA.xlsx", overwrite = T
)
write_sav(
  data = dat_sdif, path = "result/CY2_Final_SDIF_LVA.sav"
)
