library(data.table)

dat.surv <- haven::read_spss("data/Survey-Data/PRGLVAP2.sav") |> setDT()

names(dat.surv)

grep("^PV(LIT|NUM|APS)1$", names(dat.surv), value = TRUE)

dat.surv[, summary(PVLIT1)]
dat.surv[, summary(PVNUM1)]
dat.surv[, summary(PVAPS1)]
