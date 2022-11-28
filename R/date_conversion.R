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

#' Convert dates and times to SDTM-formatted ISO8601 datetime values
#'
#' @details When times are not zero-padded (for example, "5:12" instead of
#'   "05:12"), the probability that they are from a 12-hour clock instead of a
#'   24-hour clock is increased.  To minimize the impact of this, when the
#'   length of any time character string is 4 or 7 characters, times are less
#'   than the `early_hour` will trigger an error under the assumption that
#'   generally activities are not happening in the middle of the night.  If the
#'   times are known to be accurate, setting `assume_24_hr_time=TRUE` will
#'   prevent the error and simply pad the strings with zeros.
#'
#'   If a non-NA value is given for `datetime`, then values in `date` and `time`
#'   will be ignored.
#'
#' @param datetime A combined date/time representation (either an
#'   ISO8601-formatted character string or a POSIXt object).
#' @param date The date (either an ISO8601-formatted character string, a Date
#'   object, or a POSIXt object).
#' @param time The time (an ISO8601-formatted character string)
#' @param early_hour An hour of the day that would suggest times that are not
#'   zero padded are from a 12-hour clock instead of a 24-hour clock.  (See
#'   Details)
#' @param assume_24_hr_time Assume that times of day are from a 24-hour clock
#'   even if the hour is an `early_hour`.  (See Details)
#' @return An SDTM-formatted ISO8601 date-time with "UN:UN:UN" if the time is
#'   `NA`.  If all inputs are `NA`, then the output is also `NA`.
#' @export
#' @importFrom dplyr case_when
#' @importFrom lubridate format_ISO8601
generate_dtc <- function(datetime=NULL, date=NULL, time=NULL, early_hour="05", assume_24_hr_time=FALSE) {
  # Check that valid input arguments are given.
  if (is.null(datetime) & is.null(date)) {
    stop("At least one of `datetime` or `date` must be given.")
  } else if (is.null(datetime)) {
    datetime <- rep(NA_character_, length(date))
  } else if (is.null(date)) {
    date <- rep(NA_character_, length(datetime))
  }
  if (is.null(time)) {
    time <- rep(NA_character_, length(date))
  }
  if (length(datetime) != length(date)) {
    stop("`datetime` and `date` must be the same length.")
  } else if (length(time) != length(date)) {
    stop("`date` and `time` must be the same length")
  }
  # Convert non-character objects to character
  if (!is.character(datetime)) {
    datetime <- lubridate::format_ISO8601(datetime, precision="ymdhms")
  }
  if (!is.character(date)) {
    date <- lubridate::format_ISO8601(date, precision="ymd")
  }
  # Convert times that are not zero-padded to be zero-padded
  if (any(nchar(time) %in% c(4, 7))) {
    time <-
      case_when(
        nchar(time) %in% c(4, 7)~paste0("0", time),
        TRUE~time
      )
    if (!assume_24_hr_time) {
      # Check that times are not too early in the morning which may indicate that
      # times are not from a 24-hour clock.
      hour <- substr(time, 1, 2)
      if (any(!is.na(hour) & hour < early_hour)) {
        stop(
          "Some times appear to be very early in the morning (before ", early_hour,
          " hours), please confirm that these times come from a 24-hour clock.  ",
          "If they do, set `assume_24_hr_time=TRUE`."
        )
      }
    }
  }
  ret <-
    dplyr::case_when(
      !is.na(datetime)~datetime,
      !is.na(date) & !is.na(time)~paste0(date, "T", time),
      !is.na(date) & is.na(time)~paste0(date, "TUN:UN:UN"),
      TRUE~NA_character_
    )
  # Check that the output looks like an SDTM-formatted ISO8601 date
  pattern_sdtm_iso8601 <-
    paste0(
      "^",
      "(19[0-9][0-9]|20[0-9][0-9])", # yyyy requiring it to be 19xx or 20xx
      "-",
      "(0[1-9]|1[0-2])", # mm part requiring it to be 01 to 12
      "-",
      "(0[1-9]|[12][0-9]|3[0-1])", # dd part requiring it to be 01 to 31
      "T",
      "(UN|[01][0-9]|2[0-3])", # hh part requiring it to be "UN" or 00 to 23
      ":",
      "(UN|[0-5][0-9])", # mm part requiring it to be "UN" or 00 to 59
      "(:(UN|[0-5][0-9]))?", # optional ss part requiring it to be "UN" or 00 to 59
      "$"
    )
  matches <- is.na(ret) | grepl(x=ret, pattern=pattern_sdtm_iso8601)
  if (!all(matches)) {
    stop(
      "Some output does not appear to be an ISO8601 datetime formatted with SDTM unknowns: ",
      paste(ret[!matches], collapse=", ")
    )
  }
  ret
}

#' Reformat any date format (ISO8601 character string, POSIXt, or Date) to be a
#' Date object
#'
#' All values must be converted (without additional NA values created).  For
#' character strings, the year-month-day part is required to be in ISO 8601
#' format, but the (ignored) time format only requires separation by a
#' \code{"T"}.
#'
#' @param x An ISO8601 formatted character string, POSIXt object, or Date object
#' @return A Date object
#' @examples
#' dateany_to_date("2022-01-02")
#' dateany_to_date("2022-01-02T03:04") # the time part is automatically dropped
#' dateany_to_date(as.Date("2022-01-02"))
#' dateany_to_date(as.POSIXct("2022-01-02T03:04")) # the time part is still gone
#' @export
dateany_to_date <- function(x) {
  if (is.character(x)) {
    pattern_date <- "^(158[3-9]|159[0-9]|1[6-9][0-9]{2}|[2-9][0-9]{3})-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])(?:T.*)?$"
    pattern_date_match <- grepl(x = x, pattern = pattern_date) | is.na(x)
    if (!all(pattern_date_match)) {
      date_mismatch <- unique(x[!pattern_date_match])
      stop("Not all input strings match date formats: ", paste("'", date_mismatch, "'", collapse = ", ", sep = ""))
    }
    ret <- base::as.Date(x = substr(x, 1, 10), tryFormats = "%Y-%m-%d")
  } else if (is.POSIXt(x)) {
    ret <- as.Date(x)
  } else if (inherits(x, "Date")) {
    ret <- x
  } else {
    stop("Invalid input class: ", paste(class(x), collapse = ", "))
  }
  na_match <- is.na(ret) == is.na(x)
  if (!all(na_match)) {
    new_na <- unique(x[!na_match])
    stop("NAs introduced in date conversion: ", paste("'", new_na, "'", collapse = ", ", sep = ""))
  }
  ret
}

#' Calculate an SDTM --DY value from a date and a reference date
#'
#' @details \code{dates} and \code{refdt} is passed through
#'   \code{dateany_to_date()} to ensure that they are R Date class objects.
#'
#' @param dates The dates for calculation
#' @param refdt The dates for reference
#' @return An integer vector with days since the reference date according to
#'   SDTM calculation rules:  If \code{dates < refdt}, the difference in days;
#'   if \code{dates >= refdt}, the difference in days plus one.
#' @examples
#' make_dy(c("2022-01-02", "2022-01-03", "2022-01-04"), "2022-01-03")
#' @export
make_dy <- function(dates, refdt) {
  stopifnot(length(refdt) %in% c(1, length(dates)))
  dates <- dateany_to_date(dates)
  refdt <- dateany_to_date(refdt)
  ady_raw <- as.integer(dates) - as.integer(refdt)
  ady <-
    ifelse(
      ady_raw < 0,
      ady_raw,
      ady_raw + 1L
    )
  ady
}
