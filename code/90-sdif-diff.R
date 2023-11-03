# Salīdzina divus SDIF failus

library(data.table)
library(purrr)

dat_sdif_curr <- fread(file = "result/CY2_Final_SDIF_LVA.csvy", yaml = TRUE)
dat_sdif_prev <- fread(file = "result/SDIF_2023-07-26/CY2_Final_SDIF_LVA.csvy", yaml = TRUE)

dat_sdif_prev[, AGE_R := as.numeric(AGE_R)]
dat_sdif_prev[, GENDER_R := as.numeric(GENDER_R)]
dat_sdif_prev[, IMPFLGAG := as.numeric(IMPFLGAG)]
dat_sdif_prev[, IMPFLGGE := as.numeric(IMPFLGGE)]

all.equal(
  target = dat_sdif_curr,
  current = dat_sdif_prev,
  check.attributes = FALSE
)

tmp <- map2(.x = dat_sdif_curr, .y = dat_sdif_prev, .f = all.equal)

!map_lgl(tmp, is.logical)

tmp[map_lgl(tmp, is.logical)] |> unlist() |> all()

cat(names(tmp[!map_lgl(tmp, is.logical)]), sep = "\n")
