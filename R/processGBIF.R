#' processGBIF
#'
#' @param scientificName
#' @param keep
#' @param sp.code
#' @param user
#' @param pwd
#' @param email
#' @param startYear
#' @param endYear
#' @param coordPrec
#' @param centroidBufferKm
#' @param capitalBufferKm
#' @param institutionBufferKm
#'
#' @returns
#' @export
#'
#' @examples
#'
# This is a wrapper for download_gbif() and clean_gbif()
process_gbif <- function(scientificName,
                         keep,
                         sp.code,

                         # arguments to pass to download_gbif()
                         user,
                         pwd,
                         email,

                         # arguments to pass to clean_gbif()
                         startYear = 1900,
                         endYear = as.numeric(format(Sys.Date(), "%Y")),
                         coordPrec = 0.01,
                         centroidBufferKm = 2000,
                         capitalBufferKm = 2000,
                         institutionBufferKm = 2000) {


  # download
  gbif.raw <- download_gbif(scientificName = scientificName,
                            user = user, pwd = pwd, email = email)

  if (nrow(gbif.raw$dat) == 0) {
    cat("There is no GBIF data for ", scientificName, "\n")


  } else {

    # clean
    gbif.clean <- clean_gbif(gbif.raw$dat,
                             startYear, endYear,
                             coordPrec,
                             centroidBufferKm, capitalBufferKm, institutionBufferKm)

    # Keep records from desired source
    if (keep %in% c("iNat", "iNaturalist", "inat", "inaturalist")) {
      gbif.clean <- filter(gbif.clean, source == "iNat")
    } else if (keep %in% c("museum", "Museum")) {
      gbif.clean <- filter(gbif.clean, source == "Museum")
    } else {
      cat("'keep' must be 'iNaturalist' or 'Museum'")
    }

    if (nrow(gbif.clean) > 0) {

      # format for species futures
      gbif.clean <- gbif.clean %>%
        mutate(date = substr(eventDate, 1, 10),
               date = as_date(date),
               year = year(date),
               lat = decimalLatitude,
               lon = decimalLongitude,
               coord.unc = coordinateUncertaintyInMeters,
               survey.conducted = 1,
               count = 1,
               data.type = "PO",
               age = "NR",
               individual.id = NA,
               time.to.detect = NA,
               species = sp.code) %>%

        # make site.id
        group_by(lat, lon) %>%
        mutate(site.id = paste0("iNat", cur_group_id())) %>%
        ungroup() %>%


        # get survey.id
        mutate(survey.id = 1:nrow(.),
               pass.id = 1,
               survey.pass = paste0(survey.id, "_", pass.id)) %>%


        # select cols to keep
        select(site.id, lat, lon, stateProvince, coord.unc, eventDate, day, month, year, survey.conducted, survey.id, pass.id, survey.pass, data.type, species,
               age, individual.id, time.to.detect, count)

      all <- list(dat = gbif.clean,
                  citation = gbif.raw$citation)
      return(all)

    } else {
      cat("There is no data left after cleaning\n")
    }


  } # end there is iNat data for this species


}



