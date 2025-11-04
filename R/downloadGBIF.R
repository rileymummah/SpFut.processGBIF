#' Download GBIF data
#'
#' @param scientificName (character) Scientific name
#' @param startYear (numeric) First year to download data from
#' @param country (character) Country to download data from
#' @param source (character) Source to download data from (e.g. "iNaturalist")
#' @param user (character) Username for GBIF account
#' @param pwd (character) Password for GBIF account
#' @param email (character) Email address for GBIF account
#'
#' @returns A list of GBIF data and the associated citation
#' @importFrom rgbif occ_download occ_download_wait name_backbone occ_download_get pred pred_in occ_download_import gbif_citation
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- download_gbif(scientificName = "Anaxyrus macroscaphus",
#'                      user = "Username",
#'                      pwd = "Password",
#'                      email = "email@email.com")
#' }


download_gbif  <- function(scientificName,
                           startyear,
                           country = "US",
                           source = "all",
                           user,
                           pwd,
                           email) {
  
  # Get taxon key
  
  tk <- data.frame(taxon = c("Animalia", "Archaea", 
                             "Bacteria", "Chromista", "Fungi",
                             "Plantae", "Protozoa", "Viruses",
                             "Amphibia"),
                   taxonkey = c(1:8, 131))
  
  if (scientificName %in% tk$taxon){
    taxonKey <- tk$taxonkey[which(tk$taxon == scientificName)]
  } else {
    taxonKey <- rgbif::name_backbone(scientificName)$usageKey
  }
  
  
  # Download data
  if (length(taxonKey) == 0) {
    # If this species doesn't exist in GBIF
    dat <- data.frame()
    print(paste0(scientificName, " does not exist in GBIF If you do not think this is correct, check the spelling and the taxonomy."))
    
  } else {
    # if it exists, download it
    
    
    if (source == "all") {
      x <- rgbif::occ_download(
        rgbif::pred("hasGeospatialIssue", FALSE),
        rgbif::pred("hasCoordinate", TRUE),
        rgbif::pred("occurrenceStatus", "PRESENT"),
        rgbif::pred("taxonKey", taxonKey),
        rgbif::pred("country", country),
        rgbif::pred_gte("year", startyear),
        format = "SIMPLE_CSV",
        user = user, pwd = pwd, email = email)
    } else {
      x <- rgbif::occ_download(
        rgbif::pred("hasGeospatialIssue", FALSE),
        rgbif::pred("hasCoordinate", TRUE),
        rgbif::pred("occurrenceStatus", "PRESENT"),
        rgbif::pred("taxonKey", taxonKey),
        rgbif::pred("country", country),
        rgbif::pred_gte("year", startyear),
        rgbif::pred("institutionCode", source),
        format = "SIMPLE_CSV",
        user = user, pwd = pwd, email = email)
    }
    
    
    # get download status
    dlKey <- rgbif::occ_download_wait(x)
    
    
    
    # Load data
    dat <- rgbif::occ_download_get(dlKey$key) %>% rgbif::occ_download_import()
    
    
    # Remove zipped file
    rm.dir <- paste0(dlKey$key, ".zip")
    unlink(rm.dir)
    
    
    cite <- rgbif::gbif_citation(as.character(dlKey$key))[[1]]
    
  }
  
  if (nrow(dat) > 0) {
    all <- list(dat = dat,
                citation = cite)
    
    return(all)
    
  } else {
    print(paste0("No GBIF data exists for ", scientificName, "."))
    
    return(all <- list(dat = dat))
  }
}
