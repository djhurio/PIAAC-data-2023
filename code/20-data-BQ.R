# Explore BQ data

# Reset
rm(list = ls())
gc()
source(".Rprofile")

# BQ
# dat_bq_csv <- fread("data/BQ/BQ.csv")
# dat_bq_sav <- read_spss("data/BQ/BQ.sav")

# CSV
dat_bq <- fread("data/BQ/BQ.csv", dec = ",") |>
  setnames(tolower)

# Country of birth
dat_bq[, .N, keyby = .(a2_q03a, a2_q03blv)]

# Ethnicity
dat_bq[, .N, keyby = .(a2_n02lvx)]
