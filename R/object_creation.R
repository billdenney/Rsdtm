#' Create a new Rsdtm object
#' 
#' @param data A data.frame or similar object
#' @param domain The SDTM domain (as a character scalar)
#' @param ig_name The SDTM domain (as a character scalar)
#' @return An Rsdtm object with a class starting c("Rsdtm_<domain>", "Rsdtm"),
#'   then the original class
#' @export
new_Rsdtm <- function(data, domain, sdtm_version, ig_name, ig_version) {
  stopifnot(is.data.frame(data))
  stopifnot(!is_valid_sdtm_domain(domain))
  stopifnot(!is_valid_sdtm_version(sdtm_version))
  stopifnot(is.package_version(ig_version))
  list(
    data=data,
    domain=domain,
    sdtm_version=sdtm_version,
    ig_name=ig_name,
    ig_version=ig_version
  )
}

#' Check if the SDTM version, implemetation guide (IG), and IG version are valid
#' 
#' @param sdtm_version Version of the SDTM standard as a character string or `base::package_version()`
#' @param ig_name Name of the implementation guide (IG) as a character string
#' @param ig_version Version of the implementation guide as a character string or `base::package_version()`
#' @return TRUE if it is valid, FALSE if it is not valid
#' @export
is_valid_sdtm_version <- function(sdtm_version, ig_type, ig_version) {
  sdtm_mask <- valid_versions$SDTM %in% base::package_version(sdtm_version)
  ig_mask <- valid_versions$IG_NAME %in% ig_type
  ig_version_mask <- valid_versions$IG_VERSION == base::package_version(ig_version)
  any(sdtm_mask & ig_mask & ig_version_mask)
}

is_valid_sdtm_version <- function(sdtm_version) {
  stopifnot(is.package_version(sdtm_version))
  
}

is_valid_sdtm_ig_name <- function(ig_name, sdtm_version) {
  stopifnot(is.character(ig_name))
  
}

is_valid_sdtm_domain <- function(domain, sdtm_version, ig_type, ig_version) {
  stopifnot(is.character(domain))
  
}

