#' A data.frame with 4 columns and one row per valid SDTM version/IG/IG version
#' combination
#' 
#' @format
#' \itemize{
#'   \item{SDTM}{The SDTM version}
#'   \item{IG_NAME}{The implementation guide (IG) name being used}
#'   \item{IG_VERSION}{The implemetation guide (IG) version}
#'   \item{archived}{The SDTM version/IG pair has been archived (and should not
#'     be used for current studies)}
#' }
"valid_versions"
