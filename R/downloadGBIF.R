#' Download GBIF data
#'
#' @param scientificName (character) Scientific name
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
                           user,
                           pwd,
                           email) {

  # Get taxon key
  taxonKey <- rgbif::name_backbone(scientificName)$usageKey


  # Download data
  if (length(taxonKey) == 0) {
    # If this species doesn't exist in GBIF
    dat <- data.frame()
    print(paste0(scientificName, " does not exist in GBIF If you do not think this is correct, check the spelling and the taxonomy."))

  } else {
    # if it exists, download it
    x <- rgbif::occ_download(

                rgbif::pred("hasGeospatialIssue", FALSE),
                rgbif::pred("hasCoordinate", TRUE),
                rgbif::pred_in("taxonKey", taxonKey),
                format = "SIMPLE_CSV",
                user = user, pwd = pwd, email = email)

      # get download status
      dlKey <- rgbif::occ_download_wait(x)



    # Load data
    dat <- rgbif::occ_download_get(dlKey$key) %>% rgbif::occ_download_import()


    # Remove zipped file
    rm.dir <- paste0(dlKey$key, ".zip")
    unlink(rm.dir)


    cite <- rgbif::gbif_citation(as.character(dlKey$key))[[1]]

  }

  # Only keep entries that match scientific name
  dat <- dplyr::filter(dat, species == scientificName)

  if (nrow(dat) > 0) {
    all <- list(dat = dat,
                citation = cite)

    return(all)

  } else {
    print(paste0("No GBIF data exists for ", scientificName, "."))

    return(dat)
  }
}
