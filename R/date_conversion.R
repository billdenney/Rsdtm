#' Convert the character representation of the date in an original SDTM dataset
#' to a POSIXct object.
#'
#' @param x The data to convert
#' @param date_col_pattern A regex to search column names for dates to convert.
#' @param truncated Passed to \code{lubridate::ymd_hms}
#' @param ... Additional arguments passed to \code{lubridate::ymd_hms}
#' @return The data with the date converted.  Note that all dates will be
#'   returned as POSIXct objects, so partial dates will appear as the
#' @export
sdtm_dtc_to_datetime <- function(x, ...) {
  UseMethod("sdtm_dtc_to_datetime")
}

#' @rdname sdtm_dtc_to_datetime
#' @export
sdtm_dtc_to_datetime.list <- function(x, ...) {
  lapply(X=x,
         FUN=sdtm_dtc_to_datetime,
         ...)
}

#' @rdname sdtm_dtc_to_datetime
#' @export
sdtm_dtc_to_datetime.data.frame <- function(x, date_col_pattern="DTC$", truncated=5, ...) {
  date_col_names <- grep(names(x), pattern=date_col_pattern, value=TRUE)
  for (current_name in date_col_names) {
    if (!lubridate::is.POSIXt(x[[current_name]])) {
      x[[current_name]] <- ymd_hms(x[[current_name]], truncated=truncated, ...)
    } else {
      message("Column ", current_name, " is already a datetime object.")
    }
  }
  x
}
