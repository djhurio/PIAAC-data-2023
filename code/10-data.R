# Explore data

# Libs
library(data.table)
library(haven)
library(openxlsx)

# Reset
rm(list = ls())
gc()

# Functions
CalculateCD <- function(ID) {
  
  ID <- as.integer(ID)
  ID <- as.character(ID)
  
  if (min(nchar(ID)) != max(nchar(ID))) stop("All IDs should be same length")
  
  intIDLength <- nchar(ID[1])
  if (intIDLength == 0L) stop("ID lengths should be 1+")
  
  IDm <- strsplit(ID, "") |> unlist() |>
    matrix(nrow = length(ID), ncol = intIDLength, byrow = T) |> as.integer()
  
  IDw <- rep(c(3, 1), length.out = intIDLength) |> rep(times = length(ID)) |>
    matrix(nrow = length(ID), ncol = intIDLength, byrow = T)
  
  intSum <- (IDm * IDw) |> rowSums()
  
  intCheckDigit <- 10L - intSum %% 10
  intCheckDigit[intCheckDigit == 10L] <- 0L
  
  return(as.character(intCheckDigit))

}

VerifyID <- function(IDwithCD) {
  
  IDwithCD <- as.character(IDwithCD)
  
  if (min(nchar(IDwithCD)) != max(nchar(IDwithCD))) {
    stop("All IDs should be same length")
  }
  
  intIDLength <- nchar(IDwithCD[1])
  if (intIDLength < 2L) stop("ID lengths should be 2+")
  
  intCheckDigitVerify <- CalculateCD(substr(IDwithCD, 1, intIDLength - 1))
  
  return(intCheckDigitVerify == substr(IDwithCD, intIDLength, intIDLength))
  
}



# SDIF
# dat_sdif_csv <- fread("data/SDIF/SDIF.csv")
# dat_sdif_sav <- read_spss("data/SDIF/SDIF.sav")
dat_sdif_fieldw <- fread("data/SDIF/SDIF.csv")
dat_sdif_sample <- fread("data-sample/sample_piaac_sdif.csv")

setnames(dat_sdif_fieldw, tolower)
setnames(dat_sdif_sample, tolower)

dat_sdif_sample[, anyDuplicated(caseid)]
dat_sdif_fieldw[, anyDuplicated(caseid)]

x <- dat_sdif_fieldw[duplicated(caseid), caseid]
dat_sdif_fieldw[caseid %in% x]
rm(x)

tmp <- dat_sdif_fieldw[, .N, keyby = .(caseid)]
tmp[, .N, keyby = .(N)]
rm(tmp)

dat_sdif_fieldw[, sum(!is.na(persid))]

dat_sdif_fieldw[!is.na(persid)] |> View()

tab_sdif_variables <- read.xlsx(
  xlsxFile = "doc/PIAAC_CY2(2022_04)Sampling and Weighting File Layouts.xlsx",
  sheet = "SDIF"
)

names(dat_sdif_fieldw) |> sort()
tab_sdif_variables$Variable.name |> sort()

setnames(dat_sdif_fieldw, tolower)
setcolorder(dat_sdif_fieldw, tolower(tab_sdif_variables$Variable.name))
setorderv(dat_sdif_fieldw,
          tab_sdif_variables$Variable.name |> first(4) |> tolower())

dat_sdif_fieldw[, .N, keyby = .(cntryid, cntry)]

dat_sdif_fieldw[, .(cntryid, cntry, caseid, persid)]

dat_sdif_fieldw[is.na(caseid), .N]
dat_sdif_fieldw[, all(caseid %in% dat_sdif_sample$CASEID)]
dat_sdif_fieldw[, all(dat_sdif_sample$CASEID %in% caseid)]
dat_sdif_fieldw[, caseid |> table() |> table()]

dat_sdif_fieldw[!is.na(persid), all(caseid == substr(persid, 1, 8))]

dat_sdif_fieldw[, all(VerifyID(caseid))]
dat_sdif_fieldw[!is.na(persid), all(VerifyID(persid))]


dat_sdif_fieldw[, .N, keyby = . (id_majdes)]

dat_sdif_fieldw[, .N, keyby = . (id_psu)]
dat_sdif_fieldw[, .N, keyby = . (id_ssu)]

dat_sdif_sample[, .N, keyby = . (id_psu)]
dat_sdif_sample[, .N, keyby = . (id_ssu)]
