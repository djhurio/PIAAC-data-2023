# Cases with screener data

# Reset
rm(list = ls())
gc()
source(".Rprofile")

# Load data
fname <- "data/Screener/Cases_Export_20JUL2023.xlsx"
# getSheetNames(file = fname)
dat_scr <- read.xlsx(
  xlsxFile = fname, sheet = "Cases_Export", detectDates = FALSE
) |> setDT() |> setnames(tolower)
rm(fname)

# Final disposition code for household for screener
names(dat_scr)
grep("disp_scr", names(dat_scr), value = T)

unique(dat_scr[, .(caseid, disp_scr)])[, .N, keyby = .(disp_scr)]
unique(dat_scr[, .(caseid, disp_scr)])[disp_scr %in% 1:2, .N, keyby = .(disp_scr)]

dat_scr[, class(caseid)]
dat_scr[, class(persid)]

dat_scr[, .N, keyby = .(caseid)]
dat_scr[, .N, keyby = .(caseid)][, .N, keyby = .(N)]

dat_scr[, .N, keyby = .(caseid, persid)]
dat_scr[!is.na(persid), .N, keyby = .(caseid)][, .N, keyby = .(N)]
x <- dat_scr[!is.na(persid), .N, keyby = .(caseid)][N == max(N), caseid]
dat_scr[caseid %in% x, .N, keyby = .(caseid, persid)]


# PERSVAR1: Education level collected from Screener
# hihgEduCode
dat_scr[, .N, keyby = .(hihgeducode)]
dat_scr[disp_scr %in% 1:2, .N, keyby = .(hihgeducode)]
#   1: Pamatizglītība vai zemāka
#   2: Vidējā izglītība vai profesionālā/arodizglītība
#   3: Augstākā izglītība
# 980: Nezina
# 990: Atteicās atbildēt
dat_scr[, .N, keyby = .(caseid, hihgeducode)]
dat_scr[hihgeducode %in% 1:3,
        .(hihgeducode = paste(unique(sort(hihgeducode)), collapse = "-"), .N),
        keyby = .(caseid)][, .N, keyby = .(hihgeducode)]

dat_scr[hihgeducode %in% 1:3, persvar1 := as.integer(hihgeducode)]
dat_scr[, iflg_persvar1 := 0L]

dat_scr[, .N, keyby = .(!is.na(persid), persvar1, iflg_persvar1)]

dat_scr_pers <- dat_scr[!is.na(persid),
                        .(persid, persvar1, iflg_persvar1)]
saveRDS(object = dat_scr_pers, file = "data/dat_scr_pers.rds")



# DUVAR_SCRRESP1: Household size
# SCREENER_HHMEMBERS
dat_scrresp1 <- dat_scr[disp_scr %in% 1:2,
                        .(n = .N),
                        keyby = .(caseid, disp_scr, screener_hhmembers)]
dat_scrresp1
dat_scrresp1[, .N, keyby = .(screener_hhmembers)]
dat_scrresp1[, .N, keyby = .(as.numeric(screener_hhmembers))]
dat_scrresp1[, .N, keyby = .(n)]
dat_scrresp1[, .N, keyby = .(n, screener_hhmembers)]
dat_scrresp1[, cor(n, as.numeric(screener_hhmembers))]
# low cor because of 990

dat_scrresp1[!grepl("^[0-9]*$", screener_hhmembers), .N,
        keyby = .(screener_hhmembers)]
dat_scrresp1[, iflg_duvar_scrresp1 := as.integer(
  grepl("\\.", screener_hhmembers) | screener_hhmembers == "990"
)]
dat_scrresp1[, .N, keyby = .(iflg_duvar_scrresp1)]
dat_scrresp1[iflg_duvar_scrresp1 == 0, cor(n, as.numeric(screener_hhmembers))]
# 0.96 OK

dat_scrresp1[, .N, keyby = .(
  as.integer(stringr::str_extract(screener_hhmembers, "[0-9]+")),
  screener_hhmembers,
  iflg_duvar_scrresp1
)]

dat_scrresp1[, screener_hhmembers_orig := screener_hhmembers]
dat_scrresp1[, screener_hhmembers := as.integer(
  stringr::str_extract(screener_hhmembers, "[0-9]+")
)]
dat_scrresp1[, .N, keyby = .(screener_hhmembers, screener_hhmembers_orig,
                             iflg_duvar_scrresp1)]

dat_scrresp1[screener_hhmembers < 99L, duvar_scrresp1 := screener_hhmembers]
dat_scrresp1[screener_hhmembers >= 99L, duvar_scrresp1 := n]

dat_scrresp1[, .N, keyby = .(duvar_scrresp1)]
dat_scrresp1[, .N, keyby = .(duvar_scrresp1, iflg_duvar_scrresp1)]
dat_scrresp1[, .N, keyby = .(duvar_scrresp1, iflg_duvar_scrresp1,
                             screener_hhmembers_orig)]
dat_scrresp1[, .N, keyby = .(caseid, duvar_scrresp1, iflg_duvar_scrresp1)]


# DUVAR_SCRRESP2: Indicator if a child in a household
# age
dat_scr[disp_scr %in% 1:2, .N, keyby = .(age_reg)]
dat_scr[disp_scr %in% 1:2, .N, keyby = .(age)]
dat_scr[disp_scr %in% 1:2, .N, keyby = .(agerange)]

dat_scr[, class(age)]
dat_scr[, .N, keyby = .(nchar(age))]
dat_scr[nchar(age) > 3, .N, keyby = .(age)]
dat_scr[nchar(age) > 3, .(age, dobyy, dobmm,
                          as.Date(as.integer(age), origin = "1899-12-30"))]

dat_scr[!grepl("^[0-9]*$", age), .N, keyby = .(age)]

dat_scr[, iflg_duvar_scrresp2 := as.integer(
  is.na(age) | grepl("\\.", age) | (!is.na(age) & nchar(age) > 3)
)]
dat_scr[disp_scr %in% 1:2, .N, keyby = .(iflg_duvar_scrresp2)]

dat_scr[disp_scr %in% 1:2, .N, keyby = .(
  age,
  as.integer(
    map_chr(
      .x = stringr::str_extract_all(age, "[0-9]+"),
      .f = stringr::str_c,
      collapse = ""
    )
  ),
  iflg_duvar_scrresp2
)]

dat_scr[, age_orig := age]
dat_scr[nchar(age) > 3, .(age)]
dat_scr[nchar(age) > 3, age := "0"]

dat_scr[, age := as.integer(
  map_chr(
    .x = stringr::str_extract_all(age, "[0-9]+"),
    .f = stringr::str_c,
    collapse = ""
  )
)]

dat_scr[, .N, keyby = .(age, iflg_duvar_scrresp2)]
dat_scr[iflg_duvar_scrresp2 == 1, .N,
        keyby = .(age, age_orig, iflg_duvar_scrresp2)]

dat_scr[, .N, keyby = .(!is.na(persid), !is.na(age))]

dat_scrresp2 <- dat_scr[
  disp_scr %in% 1:2,
  .(duvar_scrresp2 = as.integer(any(age < 16, na.rm = TRUE)),
    iflg_duvar_scrresp2 = max(iflg_duvar_scrresp2)),
  by = .(caseid, disp_scr)
]
dat_scrresp2

dat_scrresp2[, .N, keyby = .(duvar_scrresp2)]
dat_scrresp2[, .N, keyby = .(duvar_scrresp2, iflg_duvar_scrresp2)]

dat_scrresp2[is.na(duvar_scrresp2), caseid]

dat_scr[caseid %in% dat_scrresp2[is.na(duvar_scrresp2), caseid],
        .(caseid, persid, age, dobyy, dobmm)]


# Merge
dat_scr_case <- merge(
  x = dat_scrresp1[, .(caseid, duvar_scrresp1, iflg_duvar_scrresp1)],
  y = dat_scrresp2[, .(caseid, duvar_scrresp2, iflg_duvar_scrresp2)],
  by = "caseid",
  all = TRUE
)
dat_scr_case

dat_scr_case[, .N, keyby = .(duvar_scrresp1)]
dat_scr_case[, .N, keyby = .(iflg_duvar_scrresp1)]
dat_scr_case[, .N, keyby = .(duvar_scrresp1, iflg_duvar_scrresp1)]

dat_scr_case[, .N, keyby = .(duvar_scrresp2)]
dat_scr_case[, .N, keyby = .(iflg_duvar_scrresp2)]
dat_scr_case[, .N, keyby = .(duvar_scrresp2, iflg_duvar_scrresp2)]

dat_scr_case[, .N, keyby = .(iflg_duvar_scrresp1, iflg_duvar_scrresp2)]
dat_scr_case[is.na(duvar_scrresp2)]

saveRDS(object = dat_scr_case, file = "data/dat_scr_case.rds")
