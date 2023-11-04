# Compare the last two SDIF output files

dat_sdif_prev <- fread(file = "result/SDIF_2023-10-29/CY2_Final_SDIF_LVA.csvy",
                       yaml = TRUE)
dat_sdif_curr <- fread(file = "result/CY2_Final_SDIF_LVA.csvy", yaml = TRUE)

tab_sdif_variables <- read.xlsx(
  xlsxFile = "doc/PIAAC_CY2(2022_04)Sampling and Weighting File Layouts.xlsx",
  sheet = "SDIF"
) |> setDT() |> setnames(tolower)

# dat_sdif_prev[, AGE_R := as.numeric(AGE_R)]
# dat_sdif_prev[, GENDER_R := as.numeric(GENDER_R)]
# dat_sdif_prev[, IMPFLGAG := as.numeric(IMPFLGAG)]
# dat_sdif_prev[, IMPFLGGE := as.numeric(IMPFLGGE)]

all.equal(
  target = dat_sdif_prev,
  current = dat_sdif_curr,
  check.attributes = FALSE
)

tmp <- map2(.x = dat_sdif_prev, .y = dat_sdif_curr, .f = all.equal)
# tmp[map_lgl(tmp, is.logical)] |> unlist() |> all()
# !map_lgl(tmp, is.logical)

x <- names(tmp[!map_lgl(tmp, is.logical)])
cat(x, sep = "\n")

tmp[x]

dat_sdif_prev[, ..x]
dat_sdif_curr[, ..x]

tab_sdif_variables[, .(variable.name)]
tab_sdif_variables[variable.name %in% tolower(x)]
