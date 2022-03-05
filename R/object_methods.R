#' Extract a column from an Rsdtm object
#' 
#' @param data An Rsdtm object
#' @param i The column name.  It may be a domain-agnostic column name.
#' @return The column
#' @examples
#' object[["LBTEST"]]
#' object[["--TEST"]]
#' @export
"[[.Rsdtm" <- function(data, i) {
  if (is.character(i) && startsWith(i, "--")) {
    new_i <- paste0(get_domain(data), substr(i, 3, Inf))
    data[[new_i]]
  } else {
    NextMethod(data)
  }
}

#' Get the domain of an Rsdtm object
#' 
#' @param data An Rsdtm object
#' @return The domain of the object as a character scalar
#' @export
get_domain <- function(object) {
  object[["DOMAIN"]][1]
}
