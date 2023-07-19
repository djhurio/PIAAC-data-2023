# Explore BQ data

# Reset
rm(list = ls())
gc()
source(".Rprofile")

# BQ
# dat_bq_csv <- fread("data/BQ/BQ.csv")
# dat_bq_sav <- read_spss("data/BQ/BQ.sav")

# CSV
dat_bq <- fread("data/BQ/BQ.csv", dec = ",") |> setnames(tolower)

# Sex
# A2_N02 - Vai respondents ir sieviete vai vīrietis?
# Vaicājiet tikai tad, ja neesat drošs(-a).
# <01> Vīrietis
# <02> Sieviete
dat_bq[, .N, keyby = .(a2_n02)]

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

dat_bq_raking <- dat_bq[, .(persid, a2_n02, a2_q03a, a2_q03blv, a2_n02lvx)]

saveRDS(object = dat_bq, file = "data/dat_bq.rds")
saveRDS(object = dat_bq_raking, file = "data/dat_bq_raking.rds")
