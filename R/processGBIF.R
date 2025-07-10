#' Download iNaturalist and museum data stored in GBIF and format it for Species Futures analysis
#'
#' @description A wrapper for download_gbif() and clean_gbif() to process data in one step
#'
#' @param scientificName (character vector) A vector of scientific names to search for
#' @param sp.code (character) User-provided four character species code
#' @param user (character) Username for GBIF account
#' @param pwd (character) Password for GBIF account
#' @param email (character) Email address for GBIF account
#' @param data.path (character) Location where data should be saved
#' @param citation.path (character) Location where citation should be saved
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
                         sp.code,
                         data.path = "data/",
                         citation.path = "citation/",

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
  
  
  
  # download all possible scientific names
  gbif.raw <- list(dat = c(), citation = c())
  for (n in 1:length(scinames)) {
    cat(scinames[n], "\n")
    
    gbif.raw1 <- download_gbif(scientificName = scinames[n],
                               user = user, pwd = pwd, email = email)
    
    # Only keep entries that match scientific name
    index <- which(gbif.raw1$dat$species == scinames[n] | stringr::str_to_sentence(gbif.raw1$dat$verbatimScientificName) == scinames[n])
    gbif.raw1$dat <- gbif.raw1$dat[index,]
    
    # Add to previously downloaded data
    if (nrow(gbif.raw1$dat) > 0) {
      
      # First, look for and remove duplicates
      if (length(nrow(gbif.raw$dat)) > 0) {
        old <- paste0(gbif.raw$dat$year, gbif.raw$dat$month, gbif.raw$dat$day, gbif.raw$dat$decimalLatitude, gbif.raw$dat$decimalLongitude)
        new <- paste0(gbif.raw1$dat$year, gbif.raw1$dat$month, gbif.raw1$dat$day, gbif.raw1$dat$decimalLatitude, gbif.raw1$dat$decimalLongitude)
        
        rm <- which(new %in% old)
        if (length(rm) > 0) {
          gbif.raw1$dat <- gbif.raw1$dat[-rm,]
          cat("Removing ", length(rm), " records that are likely duplicates\n")
        }
        
        if (nrow(gbif.raw1$dat) == 0) {
          cat("Adding 0 records with scientific name ", scinames[n], "\n")
          next
        }
        
      }
      
      # Then add to existing data
      gbif.raw$dat <- bind_rows(gbif.raw$dat, gbif.raw1$dat)
      cat("Adding ", nrow(gbif.raw1$dat), " records with scientific name ", scinames[n], "\n")
      
      cit <- paste0(scinames[n], ": ", gbif.raw1$citation)
      gbif.raw$citation <- c(gbif.raw$citation, cit)
      
      
    }
    
  }

  
  if (nrow(gbif.raw$dat) == 0) {
    cat("There are no GBIF data for this species.\n")


  } else {

    dat <- gbif.raw$dat
    
    # In case a record was submitted under two species names, remove duplicates
    # (This is an imperfect way to do this but it's better than nothing)
    dat <- dat %>%
      distinct(year, month, day, decimalLatitude, decimalLongitude, .keep_all = T)
    
    # Clean gbif data and split into iNat and museum
    gbif.clean <- clean_gbif(dat)
    inat <- filter(gbif.clean, source == 'iNat')
    mus <- filter(gbif.clean, source == 'Museum')
    
    if (nrow(inat) > 0) {
      
      # format for species futures
      inat <- inat %>%
        dplyr::mutate(date = substr(eventDate, 1, 10),
                      date = lubridate::as_date(date),
                      year = lubridate::year(date),
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
        dplyr::group_by(lat, lon) %>%
        dplyr::mutate(site.id = paste0(source, dplyr::cur_group_id())) %>%
        dplyr::ungroup() %>%
        
        
        # get survey.id
        dplyr::mutate(survey.id = 1:nrow(.),
                      pass.id = 1,
                      survey.pass = paste0(survey.id, "_", pass.id)) %>%
        
        
        # select cols to keep
        dplyr::select(site.id, lat, lon, stateProvince, coord.unc, eventDate,
                      day, month, year, survey.conducted, survey.id, pass.id,
                      survey.pass, data.type, species, age, individual.id,
                      time.to.detect, count)
      
      if (nrow(inat) > 0) {
        write.csv(inat, file = paste0(data.path, sp.code, "_iNat_PO.csv"), row.names = F)
        writeLines(gbif.raw$citation, paste0(citation.path, "iNat-", sp.code, ".txt"))
      }
    } else {
      cat("There are no iNaturalist records for this species")
    }
    
    if (nrow(mus) > 0) {
      
      # format for species futures
      mus <- mus %>%
        dplyr::mutate(date = substr(eventDate, 1, 10),
                      date = lubridate::as_date(date),
                      year = lubridate::year(date),
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
        dplyr::group_by(lat, lon) %>%
        dplyr::mutate(site.id = paste0(source, dplyr::cur_group_id())) %>%
        dplyr::ungroup() %>%
        
        
        # get survey.id
        dplyr::mutate(survey.id = 1:nrow(.),
                      pass.id = 1,
                      survey.pass = paste0(survey.id, "_", pass.id)) %>%
        
        
        # select cols to keep
        dplyr::select(site.id, lat, lon, stateProvince, coord.unc, eventDate,
                      day, month, year, survey.conducted, survey.id, pass.id,
                      survey.pass, data.type, species, age, individual.id,
                      time.to.detect, count)
      
      if (nrow(mus) > 0) {
        write.csv(mus, file = paste0(data.path, sp.code, "_museum_PO.csv"), row.names = F)
        writeLines(gbif.raw$citation, paste0(citation.path, "museum-", sp.code, ".txt"))
      }
      
    } else {
      cat("There are no museum records for this species")
    }
    
    rm(list=c('dat','gbif.clean','gbif.raw','gbif.raw1','inat','mus','index'))
    
    # # clean
    # gbif.clean <- clean_gbif(gbif.raw$dat,
    #                          startYear,
    #                          endYear,
    #                          centroidBufferKm, capitalBufferKm, institutionBufferKm)

    # # Keep records from desired source
    # if (keep %in% c("iNat", "iNaturalist", "inat", "inaturalist")) {
    #   gbif.clean <- dplyr::filter(gbif.clean, source == "iNat")
    # } else if (keep %in% c("museum", "Museum")) {
    #   gbif.clean <- dplyr::filter(gbif.clean, source == "Museum")
    # } else {
    #   cat("'keep' must be 'iNaturalist' or 'Museum'")
    # }
    # 
    # if (nrow(gbif.clean) > 0) {
    # 
    #   # format for species futures
    #   gbif.clean <- gbif.clean %>%
    #     dplyr::mutate(date = substr(eventDate, 1, 10),
    #                    date = lubridate::as_date(date),
    #                    year = lubridate::year(date),
    #                    lat = decimalLatitude,
    #                    lon = decimalLongitude,
    #                    coord.unc = coordinateUncertaintyInMeters,
    #                    survey.conducted = 1,
    #                    count = 1,
    #                    data.type = "PO",
    #                    age = "NR",
    #                    individual.id = NA,
    #                    time.to.detect = NA,
    #                    species = sp.code) %>%
    # 
    #     # make site.id
    #     dplyr::group_by(lat, lon) %>%
    #     dplyr::mutate(site.id = paste0(source, dplyr::cur_group_id())) %>%
    #     dplyr::ungroup() %>%
    # 
    # 
    #     # get survey.id
    #     dplyr::mutate(survey.id = 1:nrow(.),
    #                    pass.id = 1,
    #                    survey.pass = paste0(survey.id, "_", pass.id)) %>%
    # 
    # 
    #     # select cols to keep
    #     dplyr::select(site.id, lat, lon, stateProvince, coord.unc, eventDate,
    #                   day, month, year, survey.conducted, survey.id, pass.id,
    #                   survey.pass, data.type, species, age, individual.id,
    #                   time.to.detect, count)
    # 
    #   all <- list(dat = gbif.clean,
    #               citation = gbif.raw$citation)
    # 
    #   return(all)

    #} #else {
      #cat("There are no data left after cleaning\n")
    #}


  } # end there are iNat data for this species


}



