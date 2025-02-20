
user <- "clanescher"
pwd <- "0xjFyIlxJoKrNh"
email <- "clanescher@gmail.com"


# Download raw and then clean it
raw <- download_gbif(scientificName = "Anaxyrus macroscaphus",
                     user = user,
                     pwd = pwd,
                     email = email)

clean <- clean_gbif(raw = raw$dat,
                    startYear = 1980)


# OR ----
# Use the wrapper function to download raw data, clean it, and format it
sciname <- "Anaxyrus microscaphus"
sp.code <- "ANMI"

dat1 <- process_gbif(scientificName = sciname,
                     sp.code = sp.code,
                     keep = "iNat",
                     user = user,
                     pwd = pwd,
                     email = email)

dat2 <- process_gbif(scientificName = sciname,
                     sp.code = sp.code,
                     keep = "museum",
                     user = user,
                     pwd = pwd,
                     email = email)
