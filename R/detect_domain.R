#' Detect the SDTM or ADaM domain of a dataset (possibly via filename)
#' 
#' @param x The data
#' @param filename The filename associated with \code{x}
#' @return A character scalar with the name of the domain
#' @export
detect_domain <- function(x, filename) {
  domain_column_name <- intersect(c("DOMAIN", "ADDOMAIN"), names(x))
  if (length(domain_column_name) == 0) {
    domain_type <- "filename"
  } else if (length(domain_column_name) == 1) {
    name_type <- "domain"
  } else {
    stop("Unknown how to handle x with columns named both DOMAIN and ADDOMAIN")
  }
  if (nrow(x) == 0 & name_type %in% "domain") {
    warning(
      "No data in file ", filename,
      " returning the filename as the domain instead of the value in the domain column."
    )
    name_type <- "filename"
  }
  if (name_type %in% "domain") {
    make_sdtm_domain_from_domain(x)
  } else if (name_type %in% "filename") {
    make_sdtm_domain_from_filename(filename)
  }
}

#' Simplify a filename down to the SDTM list name that should be provided.
#' @param filenames The filenames to simplify.
#' @importFrom tools file_path_sans_ext
make_sdtm_domain_from_filename <- function(filenames = NULL) {
  if (is.null(filenames)) {
    stop("filenames may not be NULL.")
  } else {
    base::basename(tools::file_path_sans_ext(filenames))
  }
}

#' Extract the domain from a dataset.
#' @param data The data with a DOMAIN column for extraction
make_sdtm_domain_from_domain <- function(data) {
  domain_column_name <- intersect(c("DOMAIN", "ADDOMAIN"), names(data))
  ret <- unique(data[[domain_column_name]])
  if (length(ret) != 1) {
    stop(
      "More than one DOMAIN value cannot be present to make an SDTM list name. DOMAIN values found are: ",
      paste(ret, collapse = ", ")
    )
  }
  ret
}
