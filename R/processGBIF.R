#' Download iNaturalist and museum data stored in GBIF and format it for flexiSDM analysis
#'
#' @description A wrapper for download_gbif() and clean_gbif() to process and format data for flexiSDM
#'
#' @param scientificName (character vector) A vector of scientific names to search for
#' @param sp.code (character) User-provided species identifier; defaults to scientificName
#' @param keep (character vector) Vector including 'iNat' and/or 'museum'; default is both
#' @param user (character) Username for GBIF account
#' @param pwd (character) Password for GBIF account
#' @param email (character) Email address for GBIF account
#' @param data.path (character) Location where data should be saved; default is current directory
#' @param citation.path (character) Location where citation should be saved; default is current directory
#' @param startYear (numeric) Start year of data
#' @param endYear (numeric) End year of data
#' @param centroidBufferKm (numeric) Threshold for distance from state/county/etc centroid
#' @param capitalBufferKm (numeric) Threshold for distance from capitol
#' @param institutionBufferKm (numeric) Threshold for distance from institution (zoo, arboretum, etc.)
#'
#' @returns Writes two csv files containing iNaturalist and museum data from GBIF, and two txt files containing the associated citation(s)
#' @export
#'
#' @importFrom lubridate as_date year
#' @importFrom utils write.csv
#' @importFrom dplyr n select bind_rows filter
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' dat <- process_gbif(scientificName = "Anaxyrus microscaphus",
#'                     sp.code = "ANMI",
#'                     user = "Username",
#'                     pwd = "Password",
#'                     email = "email@email.com")
#' }


process_gbif <- function(scientificName,
                         sp.code = scientificName,
                         keep = c("iNat", "museum"),
                         data.path = "",
                         citation.path = "",

                         # arguments to pass to download_gbif()
                         user,
                         pwd,
                         email,

                         # arguments to pass to clean_gbif()
                         startYear = 1900,
                         endYear = as.numeric(format(Sys.Date(), "%Y")),
                         centroidBufferKm = 2000,
                         capitalBufferKm = 2000,
                         institutionBufferKm = 2000) {

  # clean paths
  if (substr(data.path, nchar(data.path), nchar(data.path)) != "/") paste0(data.path, "/")
  if (substr(citation.path, nchar(citation.path), nchar(citation.path)) != "/") paste0(citation.path, "/")


  # Download
  gbif.raw <- download_gbif(scientificName = scientificName, startYear = startYear,
                             user = user, pwd = pwd, email = email)
  
  
  if (nrow(gbif.raw$dat) == 0) {
    cat("There are no GBIF data for this species.\n")


  } else {

    dat <- gbif.raw$dat

    # Clean gbif data and split into iNat and museum
    gbif.clean <- clean_gbif(dat, startYear = startYear, endYear = endYear)

    if ("iNat" %in% keep) {
      inat <- dplyr::filter(gbif.clean, source == 'iNat')
    } else {
      inat <- data.frame()
    }
    if ("museum" %in% keep) {
      mus <- dplyr::filter(gbif.clean, source == 'Museum')
    } else {
      mus <- data.frame()
    }

    if (nrow(inat) > 0 & "iNat" %in% keep) {

      # format for species futures
      inat <- inat %>%
        dplyr::mutate(date = substr(.data$eventDate, 1, 10),
                      date = lubridate::as_date(date),
                      year = lubridate::year(date),
                      lat = .data$decimalLatitude,
                      lon = .data$decimalLongitude,
                      coord.unc = .data$coordinateUncertaintyInMeters,
                      survey.conducted = 1,
                      count = 1,
                      data.type = "PO",
                      age = "NR",
                      individual.id = NA,
                      time.to.detect = NA,
                      species = sp.code) %>%

        # make site.id
        dplyr::group_by(.data$lat, .data$lon) %>%
        dplyr::mutate(site.id = paste0(source, dplyr::cur_group_id())) %>%
        dplyr::ungroup() %>%


        # get survey.id
        dplyr::mutate(survey.id = 1:n(),
                      pass.id = 1,
                      survey.pass = paste0(.data$survey.id, "_", .data$pass.id)) %>%


        # select cols to keep
        dplyr::select(.data$site.id, .data$lat, .data$lon, .data$stateProvince,
                      .data$coord.unc, .data$eventDate, .data$day, .data$month,
                      .data$year, .data$survey.conducted, .data$survey.id,
                      .data$pass.id, .data$survey.pass, .data$data.type,
                      .data$species, .data$age, .data$individual.id,
                      .data$time.to.detect, .data$count)

      if (nrow(inat) > 0) {
        write.csv(inat, file = paste0(data.path, sp.code, "_iNat_PO.csv"), row.names = F)
        writeLines(gbif.raw$citation, paste0(citation.path, "iNat-", sp.code, ".txt"))
      }
    } else {
      cat("There are no iNaturalist records for this species\n")
    }

    if (nrow(mus) > 0 & "museum" %in% keep) {

      # format for species futures
      mus <- mus %>%
        dplyr::mutate(date = substr(.data$eventDate, 1, 10),
                      date = lubridate::as_date(date),
                      year = lubridate::year(date),
                      lat = .data$decimalLatitude,
                      lon = .data$decimalLongitude,
                      coord.unc = .data$coordinateUncertaintyInMeters,
                      survey.conducted = 1,
                      count = 1,
                      data.type = "PO",
                      age = "NR",
                      individual.id = NA,
                      time.to.detect = NA,
                      species = sp.code) %>%

        # make site.id
        dplyr::group_by(.data$lat, .data$lon) %>%
        dplyr::mutate(site.id = paste0(source, dplyr::cur_group_id())) %>%
        dplyr::ungroup() %>%


        # get survey.id
        dplyr::mutate(survey.id = 1:n(),
                      pass.id = 1,
                      survey.pass = paste0(.data$survey.id, "_", .data$pass.id)) %>%


        # select cols to keep
        dplyr::select(.data$site.id, .data$lat, .data$lon, .data$stateProvince,
                      .data$coord.unc, .data$eventDate, .data$day, .data$month,
                      .data$year, .data$survey.conducted, .data$survey.id,
                      .data$pass.id, .data$survey.pass, .data$data.type,
                      .data$species, .data$age, .data$individual.id,
                      .data$time.to.detect, .data$count)

      if (nrow(mus) > 0) {
        write.csv(mus, file = paste0(data.path, sp.code, "_museum_PO.csv"), row.names = F)
        writeLines(gbif.raw$citation, paste0(citation.path, "museum-", sp.code, ".txt"))
      }

    } else {
      cat("There are no museum records for this species\n")
    }

    rm(list=c('dat','gbif.clean','gbif.raw','inat','mus'))


  } # end there are iNat data for this species


}



