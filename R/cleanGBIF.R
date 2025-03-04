#' Clean GBIF data
#'
#' @description Clean raw GBIF data. Removes duplicates and coordinates which are 2km within a country centroid, a city, or a zoo/herbarium.
#'
#' @param raw (data.frame) Output from download_gbif()
#' @param startYear (numeric) Start year of data
#' @param endYear (numeric) End year of data
#' @param coordPrec (numeric) Threshold for coordinate precision
#' @param centroidBufferKm (numeric) Threshold for distance from state/county/etc centroid
#' @param capitalBufferKm (numeric) Threshold for distance from capitol
#' @param institutionBufferKm (numeric) Threshold for distance from institution (zoo, arboretum, etc.)
#'
#' @returns A cleaned GBIF dataset
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- download_gbif(scientificName = "Anaxyrus macroscaphus",
#'                      user = "Username",
#'                      pwd = "Password",
#'                      email = "email@email.com")
#'
#' clean <- clean_gbif(raw = raw$dat,
#'                     startYear = 1980)
#' }


clean_gbif <- function(raw,
                       startYear = 1900,
                       endYear = as.numeric(format(Sys.Date(), "%Y")),
                       coordPrec = 0.01,
                       centroidBufferKm = 2000,
                       capitalBufferKm = 2000,
                       institutionBufferKm = 2000) {

  ### filtering criteria from https://data-blog.gbif.org/post/gbif-filtering-guide/

  dat_clean <- raw %>%

    dplyr::filter(occurrenceStatus  == "PRESENT") %>%

    dplyr::filter(!basisOfRecord %in% c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")) %>%

    dplyr::filter(year >= startYear) %>%

    # assign NA where appropriate
    dplyr::mutate(coordinateUncertaintyInMeters = dplyr::case_when(coordinateUncertaintyInMeters %in% c(301,3036,999,9999) ~ NA,
                                                                   T ~ coordinateUncertaintyInMeters)) %>%

    # remove points at (0, 0)
    dplyr::filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%

    # remove points within 2km of country centroids
    CoordinateCleaner::cc_cen(buffer = centroidBufferKm) %>%

    # remove points within 2km of capital
    CoordinateCleaner::cc_cap(buffer = capitalBufferKm) %>%

    # remove points within 2km of zoo/herbarium
    CoordinateCleaner::cc_inst(buffer = institutionBufferKm) %>%

    # remove points that are in the ocean
    CoordinateCleaner::cc_sea() %>%

    # remove duplicates
    dplyr::distinct(decimalLongitude, decimalLatitude, speciesKey, datasetKey, .keep_all = TRUE)  %>%


    dplyr::filter(decimalLongitude < 0)


  # separate counts into individual rows
  dat_clean$individualCount[which(is.na(dat_clean$individualCount))] <- 1
  dat_clean <- dat_clean %>% tidyr::uncount(individualCount)

  # clean up
  dat <- raw %>%

    # categorize as iNat or Museum
    dplyr::mutate(source = dplyr::case_when(institutionCode == "iNaturalist" ~ "iNat",
                                             basisOfRecord == "PRESERVED_SPECIMEN" ~ "Museum",
                                             T ~ "Other"),


           # whether to include in "clean" dataset or not
           incl = dplyr::case_when(gbifID %in% dat_clean$gbifID &
                                     source %in% c("iNat", "Museum") ~ 1,
                                   T ~ 0)) %>%
    dplyr::filter(incl == 1)



  return(dat)

}

#End script
