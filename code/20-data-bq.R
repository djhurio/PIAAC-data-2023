# Explore BQ data

# Reset
rm(list = ls())
gc()

# BQ

# Current
dat_bq <- fread(file = "data/BQ/BQ_3NOV2023.csv", dec = ",") |>
  setnames(tolower)

# Previous
dat_bq_prev <- fread(file = "data/BQ/BQ_18JUL2023.csv", dec = ",") |>
  setnames(tolower)

# Compare
all.equal(dat_bq, dat_bq_prev)

tmp <- map2(.x = dat_bq, .y = dat_bq_prev, .f = all.equal)
tmp[map_lgl(tmp, is.logical)] |> unlist() |> all()
# cat(names(tmp[!map_lgl(tmp, is.logical)]), sep = "\n")

tmp <- merge(
  x = dat_bq[,      .(persid, a2_n02, a2_d01b)],
  y = dat_bq_prev[, .(persid, a2_n02, a2_d01b)],
  by = "persid",
  suffixes = c(".curr", ".prev")
)

tmp[, all.equal(a2_n02.curr, a2_n02.prev)]
tmp[, all.equal(a2_d01b.curr, a2_d01b.prev)]

tmp[, .N, keyby = .(a2_n02.curr, a2_n02.prev)]
tmp[, .N, keyby = .(a2_d01b.curr, a2_d01b.prev)]

rm(dat_bq_prev)
rm(tmp)



dat_bq[, class(persid)]
dat_bq[, persid := as.numeric(persid)]
dat_bq[, class(persid)]

# Sex
# A2_N02 - Vai respondents ir sieviete vai vīrietis?
# Vaicājiet tikai tad, ja neesat drošs(-a).
# <01> Vīrietis
# <02> Sieviete
dat_bq[, .N, keyby = .(a2_n02)]

# Age
dat_bq[, .N, keyby = .(a2_d01b)]

# Country of birth
# A2_Q03a - Vai esat dzimis(-usi) Latvijā?
# <01> Jā
# <02> Nē
dat_bq[, .N, keyby = .(a2_q03a)]

# A2_Q03bLV - Kurā valstī esat dzimis(-usi)?
# Lūdzu, norādiet PAŠREIZĒJO valsts nosaukumu.
# <01> Krievija
# <02> Baltkrievija
# <03> Ukraina
# <04> Polija
# <05> Igaunija
# <06> Lietuva
# <07> Kazahstāna
# <08> Vācija
# <09> Citā valstī
dat_bq[, .N, keyby = .(a2_q03a, a2_q03blv)]

# Ethnicity
# A2_N02LVX - Kāda ir Jūsu tautība?
# <01> Latviešu
# <02> Krievu
# <03> Ukraiņu
# <04> Baltkrievu
# <05> Igauņu
# <06> Lietuviešu
# <07> Poļu
# <08> Ebreju
# <09> Romu
# <10> Cits
dat_bq[, .N, keyby = .(a2_n02lvx)]

dat_bq_raking <- dat_bq[, .(
  persid,
  a2_n02, # Sex
  a2_d01b, # Age
  a2_q03a, # Vai esat dzimis(-usi) Latvijā?
  a2_q03blv, # Kurā valstī esat dzimis(-usi)?
  a2_n02lvx # Kāda ir Jūsu tautība?
)]

saveRDS(object = dat_bq, file = "data/dat_bq.rds")
saveRDS(object = dat_bq_raking, file = "data/dat_bq_raking.rds")
