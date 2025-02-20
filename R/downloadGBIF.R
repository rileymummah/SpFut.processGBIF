#' Download GBIF data
#'
#' @param scientificName (character) Scientific name
#' @param user (character) Username for iNaturalist account
#' @param pwd (character) Password for iNaturalist account
#' @param email (character) Email address for iNaturalist account
#'
#' @returns A list of GBIF data and the associated citation
#' @export
#'
#' @examples
#' raw <- download_gbif(scientificName = "Anaxyrus macroscaphus",
#'                      user = "Username",
#'                      pwd = "Password",
#'                      email = "email@email.com")


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
    cat(scientificName, " does not exist in GBIF If you do not think this is correct, check the spelling and the taxonomy.")

  } else {
    # if it exists, download it
    dlKey <- rgbif::occ_download(

      rgbif::pred("hasGeospatialIssue", FALSE),
      rgbif::pred("hasCoordinate", TRUE),
      rgbif::pred_in("taxonKey", taxonKey),
      format = "SIMPLE_CSV",
      user = user, pwd = pwd, email = email) %>%

      # get download status
      rgbif::occ_download_wait()



    # Load data
    dat <- rgbif::occ_download_get(dlKey$key) %>% rgbif::occ_download_import()


    # Remove zipped file
    rm.dir <- paste0(dlKey$key, ".zip")
    unlink(rm.dir)


    # # Write citation info
    # if (substr(cite.path, nchar(cite.path), nchar(cite.path)) == "/") cite.path <- substr(cite.path, 1, nchar(cite.path)-1)
    #
    # if (dir.exists(cite.path) == F) {
    #   dir.create(cite.path)
    # }

    cite <- rgbif::gbif_citation(as.character(dlKey$key))[[1]]
    #writeLines(cite, paste0(cite.path, "/", cite.label, ".txt"))
  }

  all <- list(dat = dat,
              citation = cite)

  return(all)
}
