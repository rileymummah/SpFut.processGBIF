#' Download GBIF data
#'
#' @param scientificName (character) Scientific name
#' @param country (character) Country to download data from
#' @param source (character) Source to download data from (e.g. "iNaturalist")
#' @param user (character) Username for GBIF account
#' @param pwd (character) Password for GBIF account
#' @param email (character) Email address for GBIF account
#' @param taxonKey (numeric) Taxon key representing the species of interest in GBIF. Default is "unknown", meaning taxon key will be searched for based on Scientific Name
#'
#' @returns A list of GBIF data and the associated citation
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rgbif occ_download occ_download_wait name_backbone occ_download_get pred pred_in occ_download_import gbif_citation name_usage
#'
#' @examples
#' \dontrun{
#' raw <- download_gbif(scientificName = "Anaxyrus macroscaphus",
#'                      country = "US",
#'                      source = "all",
#'                      user = "Username",
#'                      pwd = "Password",
#'                      email = "email@email.com",
#'                      taxonKey = "unknown")
#' }


download_gbif  <- function(scientificName,
                           country = "US",
                           source = "all",
                           user,
                           pwd,
                           email,
                           taxonKey = "unknown") {

  # Get taxon key

  
  if (taxonKey == "unknown") {
    tk <- data.frame(taxon = c("Animalia", "Archaea",
                               "Bacteria", "Chromista", "Fungi",
                               "Plantae", "Protozoa", "Viruses",
                               "Amphibia"),
                     taxonkey = c(1:8, 131))
    
    if (scientificName %in% tk$taxon){
      taxonKey <- tk$taxonkey[which(tk$taxon == scientificName)]
    } else {
      cat("Searching for taxonkey for ", scientificName, "\n")
      taxonKey <- rgbif::name_backbone(scientificName)$usageKey
      cat("Using taxonkey ", taxonKey, "\n")
    }
  }
  
  # Check taxonkey
  sciname <- rgbif::name_usage(key = taxonKey)
  cat("Downloading data for", sciname$data$canonicalName, "\n")

  # Download data
  if (length(taxonKey) == 0) {
    # If this species doesn't exist in GBIF
    dat <- data.frame()
    cat(scientificName, " does not exist in GBIF. If you do not think this is correct, check the spelling and the taxonomy, or input the taxonkey directly with the `taxonkey` argument.\n")

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
    } else if (source == "iNaturalist") {
      x <- rgbif::occ_download(
        rgbif::pred("hasGeospatialIssue", FALSE),
        rgbif::pred("hasCoordinate", TRUE),
        rgbif::pred("occurrenceStatus", "PRESENT"),
        rgbif::pred("taxonKey", taxonKey),
        rgbif::pred("country", country),
        rgbif::pred_gte("year", startyear),
        #rgbif::pred("institutionCode", "iNaturalist"),
        pred("datasetKey", "50c9509d-22c7-4a22-a47d-8c48425ef4a7"), # According to John Waller on 12/3/2025, this is more accurate than institutionCode == "iNaturalist"
        format = "SIMPLE_CSV",
        user = user, pwd = pwd, email = email)
    } else {
      cat("source must be 'all' or 'iNaturalist'")
      break
    }


    # get download status
    dlKey <- rgbif::occ_download_wait(x)



    # Load data
    dat <- rgbif::occ_download_get(dlKey$key) %>% rgbif::occ_download_import()

    # Only keep entries that match scientific name
    index <- which(dat$species == scientificName | stringr::str_to_sentence(dat$verbatimScientificName) == scientificName)
    dat <- dat[index,]

    # Remove zipped file
    rm.dir <- paste0(dlKey$key, ".zip")
    unlink(rm.dir)


    cite <- rgbif::gbif_citation(as.character(dlKey$key))[[1]]

  }

  if (nrow(dat) > 0) {
    all <- list(dat = dat,
                citation = cite)

    cat("Returning GBIF data for ", scientificName, "\n")
    
    return(all)

  } else {
    cat("No GBIF data exists for ", scientificName, ".\n")

    return(all <- list(dat = dat))
  }
}
