# SDIF test

# Reset
rm(list = ls())
gc()
source(".Rprofile")

# SDIF load

dat_sdif <- fread("result/CY2_Final_SDIF_LVA.csvy", yaml = TRUE) |>
  setnames(tolower)
# dat_sdif <- fread("result/CY2_Final_SDIF_LVA.csv", yaml = FALSE) |>
#   setnames(tolower)

dat_sdif[, length(unique(caseid))]
