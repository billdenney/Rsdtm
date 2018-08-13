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
  ret <- make_sdtm_domain_from_domain(x)
  if (is.null(ret)) {
    ret <- make_sdtm_domain_from_filename(filename)
    if (verbose) {
      message("Detected domain ", ret, " from filename.")
    }
  } else if (verbose) {
    message("Detected domain ", ret, " from data.")
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
  if (nrow(data) == 0) {
    # TODO: detect based on column names present
    return(NULL)
  }
  domain_column_name <- intersect(c("DOMAIN", "ADDOMAIN", "RDOMAIN"), names(data))
  if (length(domain_column_name) == 0) {
    return(NULL)
  } else if (length(domain_column_name) == 1) {
    # Proceed
  } else if (length(domain_column_name) > 1) {
    # Determine the right column for the domain
    if ("DOMAIN" %in% domain_column_name) {
      domain_column_name <- "DOMAIN"
    } else {
      stop("Multiple columns that may define the domain are present (",
           paste(domain_column_name, collapse=", "),
           "); uncertain how to proceed.")
    }
  }
  ret <- unique(data[[domain_column_name]])
  if (length(ret) != 1) {
    stop(
      "More than one domain value cannot be present to make an SDTM name. ",
      domain_column_name,
      " values found are: ",
      paste(ret, collapse = ", ")
    )
  }
  if (domain_column_name %in% "RDOMAIN") {
    # Handle supplementary datasets
    ret <- paste0("SUPP", ret)
  }
  ret
}
