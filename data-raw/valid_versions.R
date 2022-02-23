#' Create the structure for a new, valid SDTM version
#' 
#' @inherit is_valid_sdtm_version
#' @return A tibble with columns for the above information
#' @keywords Internal
new_valid_sdtm_version <- function(sdtm_version, ig_name, ig_version, archived=FALSE) {
  tibble::tibble(
    SDTM=base::package_version(sdtm_version),
    IG_NAME=ig_name,
    IG_VERSION=base::package_version(ig_version),
    archived=archived
  )
}

valid_versions <-
  dplyr::bind_rows(
    new_valid_sdtm_version(sdtm_version=1.1, ig_name="SDTMIG", ig_version="3.1.1", archived=TRUE),
    new_valid_sdtm_version(sdtm_version=1.2, ig_name="SDTMIG", ig_version="3.1.2", archived=TRUE),
    new_valid_sdtm_version(sdtm_version=1.3, ig_name="SDTMIG", ig_version="3.1.3", archived=TRUE),
    new_valid_sdtm_version(sdtm_version=1.3, ig_name="SDTMIG-MD", ig_version="1.0", archived=TRUE),
    
    new_valid_sdtm_version(sdtm_version=1.4, ig_name="SDTMIG", ig_version=3.2),
    new_valid_sdtm_version(sdtm_version=1.4, ig_name="SDTMIG-AP", ig_version="1.0"),
    new_valid_sdtm_version(sdtm_version=1.5, ig_name="SENDIG", ig_version=c("3.1", "3.1.1")),
    new_valid_sdtm_version(sdtm_version=1.6, ig_name="SENDIG-DART", ig_version=1.1),
    new_valid_sdtm_version(sdtm_version=1.7, ig_name="SDTMIG", ig_version=3.3),
    new_valid_sdtm_version(sdtm_version=1.8, ig_name="SENDIG-AR", ig_version="1.0"),
    new_valid_sdtm_version(sdtm_version="2.0", ig_name="SDTMIG", ig_version=3.4)
  )

usethis::use_data(valid_versions, overwrite=TRUE)
