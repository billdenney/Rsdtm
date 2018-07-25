#' Simplify the names of an SDTM data.frame to remove the DOMAIN from name
#' prefixes.
#' @param x A data.frame of SDTM data from a single domain.
#' @return \code{x} with the DOMAIN removed from names.
#' @details WARNING: Currently, this is implemented by looking at the domain and
#'   stripping those characters from the beginning of any column names.  If
#'   there are column names that incidentally start with the same two
#'   characters, the initial two characters will be removed in error (please
#'   report that as a bug).
#' @export
#' @importFrom dplyr rename_all
simplify_sdtm_names <- function(x) {
  drop_domain <- function(y, domain) {
    mask_drop <- substr(y, 1, 2) == domain
    if (any(mask_drop)) {
      y[mask_drop] <- substr(y[mask_drop], 3, 1000)
    }
    y
  }
  rename_all(.tbl=x, .funs=drop_domain, domain=unique(x$DOMAIN))
}
