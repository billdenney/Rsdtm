#' Detect the SDTM or ADaM domain of a dataset (possibly via filename)
#' 
#' @param x The data
#' @param filename The filename associated with \code{x}
#' @return A character scalar with the name of the domain
#' @export
detect_domain <- function(x, filename, verbose=TRUE) {
  if ("RELID" %in% names(x)) {
    return("RELREC")
  }
  domain_column_name <- intersect(c("DOMAIN", "ADDOMAIN", "RDOMAIN"), names(x))
  if (length(domain_column_name) == 0) {
    name_type <- "filename"
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
    ret <- make_sdtm_domain_from_domain(x)
    if (verbose) {
      message("Detected ", ret, " from domain column.")
    }
  } else if (name_type %in% "filename") {
    ret <- make_sdtm_domain_from_filename(filename)
    if (verbose) {
      message("Detected ", ret, " from filename.")
    }
  }
  ret
}

#' Simplify a filename down to the SDTM list name that should be provided.
#' @param filenames The filenames to simplify.
#' @importFrom tools file_path_sans_ext
make_sdtm_domain_from_filename <- function(filenames = NULL) {
  if (is.null(filenames)) {
    stop("filenames may not be NULL.")
  } else {
    toupper(base::basename(tools::file_path_sans_ext(filenames)))
  }
}

#' Extract the domain from a dataset.
#' @param data The data with a DOMAIN column for extraction
make_sdtm_domain_from_domain <- function(data) {
  domain_column_name <- intersect(c("DOMAIN", "ADDOMAIN", "RDOMAIN"), names(data))
  ret <- unique(data[[domain_column_name]])
  if (length(ret) != 1) {
    stop(
      "More than one DOMAIN value cannot be present to make an SDTM list name. DOMAIN values found are: ",
      paste(ret, collapse = ", ")
    )
  }
  if (domain_column_name %in% "RDOMAIN") {
    # Handle supplementary datasets
    ret <- paste0("SUPP", ret)
  }
  ret
}
