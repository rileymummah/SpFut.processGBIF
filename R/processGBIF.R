#' Download iNaturalist and museum data stored in GBIF and format it for flexiSDM analysis
#'
#' @description A wrapper for download_gbif() and clean_gbif() to process and format data for flexiSDM
#'
#' @param scientificName (character vector) A vector of scientific names to search for
#' @param sp.code (character) User-provided species identifier; defaults to scientificName
#' @param keep (character vector) Vector including 'iNaturalist' and/or 'museum'; default is both
#' @param user (character) Username for GBIF account
#' @param pwd (character) Password for GBIF account
#' @param email (character) Email address for GBIF account
#' @param out (character) How output should be saved; defaults to 'save' (output is saved to data.path and citation.path), alternative is 'return' (output is returned as a list)
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
#' @importFrom rlang .data
#' @importFrom lubridate as_date year
#' @importFrom utils write.csv
#' @importFrom dplyr n bind_rows filter ungroup group_by cur_group_id select
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
                         keep = c("iNaturalist", "museum"),
                         out = "save",
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

  ret <- list()
  
  # check keep argument
  diff <- setdiff(keep, c("iNaturalist", "museum"))
  if (length(diff) > 0) stop("Keep must be 'iNaturalist', 'museum', or both")
  
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

    if ("iNaturalist" %in% keep) {
      inat <- filter(gbif.clean, source == 'iNat')
    } else {
      inat <- data.frame()
    }
    if ("museum" %in% keep) {
      mus <- filter(gbif.clean, source == 'Museum')
    } else {
      mus <- data.frame()
    }

    if (nrow(inat) > 0 & "iNaturalist" %in% keep) {

      # format for species futures
      inat <- inat %>%
        mutate(date = substr(.data$eventDate, 1, 10),
               date = suppressWarnings(as_date(date)), # Will not report failure to parse
               year = year(date),
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
        group_by(.data$lat, .data$lon) %>%
        mutate(site.id = paste0(source, cur_group_id())) %>%
        ungroup() %>%


        # get survey.id
        mutate(survey.id = 1:n(),
               pass.id = 1,
               survey.pass = paste0(.data$survey.id, "_", .data$pass.id)) %>%


        # select cols to keep
        select("site.id", "lat", "lon", "stateProvince", "coord.unc",
               "eventDate", "day", "month", "year", "survey.conducted",
               "survey.id", "pass.id", "survey.pass", "data.type", "species",
               "age", "individual.id", "time.to.detect", "count")

      if (nrow(inat) > 0 & out == "save") {
        write.csv(inat, file = paste0(data.path, sp.code, "_iNat_PO.csv"), row.names = F)
        writeLines(gbif.raw$citation, paste0(citation.path, "iNat-", sp.code, ".txt"))
      } else if (nrow(inat) > 0 & out == "return") {
        ret[["iNaturalist"]] <- list(dat = inat, citation = gbif.raw$citation)
      }
    } else {
      if ("iNaturalist" %in% keep) cat("There are no iNaturalist records for this species\n")
    }

    if (nrow(mus) > 0 & "museum" %in% keep) {

      # format for species futures
      mus <- mus %>%
        mutate(date = substr(.data$eventDate, 1, 10),
               date = suppressWarnings(as_date(date)), # Will not report failure to parse
               year = year(date),
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
        group_by(.data$lat, .data$lon) %>%
        mutate(site.id = paste0(source, cur_group_id())) %>%
        ungroup() %>%


        # get survey.id
        mutate(survey.id = 1:n(),
               pass.id = 1,
               survey.pass = paste0(.data$survey.id, "_", .data$pass.id)) %>%


        # select cols to keep
        select("site.id", "lat", "lon", "stateProvince", "coord.unc",
               "eventDate", "day", "month", "year", "survey.conducted",
               "survey.id", "pass.id", "survey.pass", "data.type", "species",
               "age", "individual.id", "time.to.detect", "count")

      if (nrow(mus) > 0 & out == "save") {
        write.csv(mus, file = paste0(data.path, sp.code, "_museum_PO.csv"), row.names = F)
        writeLines(gbif.raw$citation, paste0(citation.path, "museum-", sp.code, ".txt"))
      } else if (nrow(inat) > 0 & out == "return") {
        ret[["museum"]] <- list(dat = mus, citation = gbif.raw$citation)
      }

    } else {
      if ("museum" %in% keep) cat("There are no museum records for this species\n")
    }

    rm(list=c('dat','gbif.clean','gbif.raw','inat','mus'))


  } # end there are iNat data for this species

  if (out == "return") return(ret)

}



