## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SpFut.processGBIF)

user <- "myname"
pwd <- "MyFancyPassword"
email <- "myname@gmail.com"

## ----real-login, echo=F-------------------------------------------------------
user <- "rmummah"
pwd <- "M@gicPizza1"
email <- "rileymummah@gmail.com"

## ----download-clean-----------------------------------------------------------
raw <- download_gbif(scientificName = "Anaxyrus microscaphus",
                     user = user,
                     pwd = pwd,
                     email = email)

head(raw$dat)

raw$citation


clean <- clean_gbif(raw = raw$dat,
                    startYear = 1980)

head(clean)

colnames(clean)

## ----processGBIF-iNat---------------------------------------------------------
sciname <- "Anaxyrus microscaphus"
# This is a user-input, which is saved in the output
sp.code <- "ANMI"

# To download iNat data
iNat <- process_gbif(scientificName = sciname,
                     sp.code = sp.code,
                     keep = "iNat",
                     user = user,
                     pwd = pwd,
                     email = email)

head(iNat)

## ----processGBIF-museum-------------------------------------------------------
museum <- process_gbif(scientificName = sciname,
                       sp.code = sp.code,
                       keep = "museum",
                       user = user,
                       pwd = pwd,
                       email = email)

head(museum)

