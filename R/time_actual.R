#' Generate actual times from dates and times
#'
#' @param x An SDTM data set
#' @param ref_col The character name of the column to use as a reference time
#'   point.
#' @param dtc_col_pattern The regex to use to select columns to use for actual
#'   time generation.  The regex will also be used to modify the name.
#' @param units The units to report the actual time in (passed to \code{as.numeric}).
#' @param tz The time zone for difftime (default is used if \code{is.null(tz)}).
#' @param ... Passed to methods.
#' @return \code{x} with new columns matching \code{dtc_col_pattern} named
#'   \code{gsub(pattern=dtc_col_pattern, replacement=paste0("actual_", units),
#'   names(x))} The new columns will have the actual time in numerical values
#'   matching the \code{units}.
#' @export
sdtm_time_actual <- function(x, ...) {
  UseMethod("sdtm_time_actual")
}

#' @rdname sdtm_time_actual
#' @export
sdtm_time_actual.list <- function(x, ...) {
  lapply(X=x,
         FUN=sdtm_time_actual,
         ...)
}

#' @rdname sdtm_time_actual
#' @export
sdtm_time_actual.data.frame <- function(x, ref_col="DTC_first_dose", dtc_col_pattern="DTC$", units="hours", tz=NULL, ...) {
  dtc_cols <- grep(pattern=dtc_col_pattern, x=names(x), value=TRUE)
  for (current_name in dtc_cols) {
    new_name <- gsub(pattern=dtc_col_pattern, replacement=paste0("_actual_", units), x=current_name)
    if (is.null(tz)) {
      x[[new_name]] <- difftime(time1=x[[current_name]], time2=x[[ref_col]])
    } else {
      x[[new_name]] <- difftime(time1=x[[current_name]], time2=x[[ref_col]], tz=tz)
    }
    x[[new_name]] <- as.numeric(x[[new_name]], units=units)
  }
  x
}
