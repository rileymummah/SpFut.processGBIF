#' Title
#'
#' @param raw
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
clean_gbif <- function(raw,                                                 # output from download_gbif()
                       startYear = 1900,                                    # start year for data
                       endYear = as.numeric(format(Sys.Date(), "%Y")),      # end year for data
                       coordPrec = 0.01,                                    # threshold for coordinate precision
                       centroidBufferKm = 2000,                             # threshold for distance from state/county/etc centroid
                       capitalBufferKm = 2000,                              # threshold for distance from capital
                       institutionBufferKm = 2000) {                        # threshold for distance from institution (zoo, arboretum, etc.)

  ### filtering criteria from https://data-blog.gbif.org/post/gbif-filtering-guide/

  dat_clean <- raw %>%

    filter(occurrenceStatus  == "PRESENT") %>%

    filter(!basisOfRecord %in% c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")) %>%

    filter(year >= startYear) %>%

    # assign NA where appropriate
    mutate(coordinateUncertaintyInMeters = case_when(coordinateUncertaintyInMeters %in% c(301,3036,999,9999) ~ NA,
                                                     T ~ coordinateUncertaintyInMeters)) %>%

    # remove points at (0, 0)
    filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%

    # remove points within 2km of country centroids
    cc_cen(buffer = centroidBufferKm) %>%

    # remove points within 2km of capital
    cc_cap(buffer = capitalBufferKm) %>%

    # remove points within 2km of zoo/herbarium
    cc_inst(buffer = institutionBufferKm) %>%

    # remove points that are in the ocean
    cc_sea() %>%

    # remove duplicates
    distinct(decimalLongitude, decimalLatitude, speciesKey, datasetKey, .keep_all = TRUE)  %>%


    filter(decimalLongitude < 0)


  # separate counts into individual rows
  dat_clean$individualCount[which(is.na(dat_clean$individualCount))] <- 1
  dat_clean <- dat_clean %>%
    uncount(individualCount)

  # clean up
  dat <- raw %>%

    # categorize as iNat or Museum
    mutate(source = case_when(institutionCode == "iNaturalist" ~ "iNat",
                              basisOfRecord == "PRESERVED_SPECIMEN" ~ "Museum",
                              T ~ "Other"),


           # whether to include in "clean" dataset or not
           incl = case_when(gbifID %in% dat_clean$gbifID &
                              source %in% c("iNat", "Museum") ~ 1,
                            T ~ 0)) %>%
    filter(incl == 1)



  return(dat)

}

#End script
