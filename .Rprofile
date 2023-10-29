# renv
options(renv.config.ppm.enabled = TRUE)
options(renv.config.pak.enabled = TRUE)
options(renv.config.ppm.default = TRUE)
source("renv/activate.R")

# libs
library(purrr)
library(data.table)
library(haven)
library(openxlsx)
library(ggplot2)

# options
options(datatable.integer64 = "character")
options(datatable.na.strings = "")

# functions
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
