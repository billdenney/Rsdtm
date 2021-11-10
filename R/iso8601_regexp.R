#' Determine if a string matches an ISO 8601 date/time using standard calendar
#' date notation
#' 
#' @param x A vector of character strings to test
#' @inheritDotParams pattern_ISO8601_calendar_datetime
#' @family ISO8601 String checking
#' @export
is_ISO8601_calendar_datetime <- function(x, ...) {
  grepl(
    x=as.character(x),
    pattern=make_full_pattern(pattern_ISO8601_calendar_datetime(...))
  )
}

#' Determine if a string is an ISO 8601 date
#' 
#' @inheritParams is_ISO8601_calendar_datetime
#' @inheritDotParams pattern_ISO8601_calendar_date
#' @family ISO8601 String checking
#' @export
is_ISO8601_calendar_date <- function(x, ...) {
  grepl(
    x=as.character(x),
    pattern=make_full_pattern(pattern_ISO8601_calendar_date(...))
  )
}

#' Determine if a string is an ISO 8601 time
#' 
#' @inheritParams is_ISO8601_calendar_datetime
#' @inheritDotParams pattern_ISO8601_time
#' @family ISO8601 String checking
#' @export
is_ISO8601_time <- function(x, ...) {
  grepl(
    x=as.character(x),
    pattern=make_full_pattern(pattern_ISO8601_time(...))
  )
}

#' Determine if a string is an ISO 8601 timezone
#' 
#' @inheritParams is_ISO8601_calendar_datetime
#' @family ISO8601 String checking
#' @export
is_ISO8601_timezone <- function(x) {
  grepl(
    x=as.character(x),
    pattern=make_full_pattern(pattern_ISO8601_timezone())
  )
}

#' Generate a regular expression matching an ISO8601 calendar date with time
#'
#' @param truncated Should the date/time be allowed to be truncated?  An integer
#'   indicating the highest required precision (0=second is required, 1=minute,
#'   2=hour, 3=day, 4=month, 5=year, 6=none).  A value of 6 will allow an empty
#'   string to match.
#' @param ... Passed to \code{pattern_ISO8601_calendar_date()} and
#'   \code{pattern_ISO8601_time()}
#' @family ISO8601 patterns
#' @export
pattern_ISO8601_calendar_datetime <- function(truncated=0, ...) {
  stopifnot(is.numeric(truncated))
  stopifnot(length(truncated) == 1)
  stopifnot(truncated >= 0 & truncated <= 6)
  # Min truncated is 2 because 3 will be handled below with the addition of the
  # "T"
  pattern_time <- pattern_ISO8601_time(truncated=min(truncated, 3), require_T=TRUE, ...)
  pattern_time_aug <-
    pattern_ISO8601_truncated_helper(
      pattern_time,
      allow_truncation=truncated >= 3
    )
  pattern_ISO8601_calendar_date(
    truncated=max(truncated - 3, 0),
    time_pattern=pattern_time,
    ...
  )
}

#' Generate a regular expression matching an ISO8601 date
#' 
#' @details Basic format (without dashes) is not supported.  Sign on the year (+
#'   or -) is not supported (therefore years before 0000 are not supported).
#'   Years after 9999 are not supported.
#' 
#' @param truncated Should the date be allowed to be truncated?  An integer
#'   indicating the highest required precision (0=day is required, 1=month, and
#'   2=year).  A value of 3 will allow an empty string to match.
#' @param allow_before_year_1583 Should years between 0 and 1582 be allowed
#'   (they are only allowed in ISO 8601 with mutual agreement)
#' @param time_pattern A string to add to the day for including time with date
#'   (see \code{pattern_ISO8601_time()})
#' @param ... Ignored
#' @references https://en.wikipedia.org/wiki/ISO_8601
#' @family ISO8601 patterns
#' @export
pattern_ISO8601_calendar_date <- function(truncated=0, allow_before_year_1583=FALSE, time_pattern="", ...) {
  stopifnot(is.numeric(truncated))
  stopifnot(length(truncated) == 1)
  stopifnot(truncated >= 0 & truncated <= 3)

  stopifnot(is.logical(allow_before_year_1583))
  stopifnot(!is.na(allow_before_year_1583))
  
  stopifnot(is.character(time_pattern))
  stopifnot(length(time_pattern) == 1)
  stopifnot(!is.na(time_pattern))

  pattern_year <-
    if (allow_before_year_1583) {
      "([0-9]{4})"
    } else {
      "(158[3-9]|159[0-9]|1[6-9][0-9]{2}|[2-9][0-9]{3})"
    }
  pattern_month <- "-(0[1-9]|1[0-2])"
  # Not confirming that the date is valid for the month
  pattern_day_number <- "0[1-9]|[12][0-9]|3[01]"
  pattern_day <-
    if (nchar(time_pattern) == 0) {
      sprintf("-(%s)", pattern_day_number)
    } else {
      sprintf("-(?:(%s)%s)", pattern_day_number, time_pattern)
    }
  # Put it together
  pattern_day_aug <- pattern_ISO8601_truncated_helper(pattern_day, allow_truncation=truncated >= 1)
  pattern_month_aug <-
    pattern_ISO8601_truncated_helper(
      paste0(pattern_month, pattern_day_aug),
      allow_truncation=truncated >= 2
    )
  pattern_year_aug <-
    pattern_ISO8601_truncated_helper(
      paste0(pattern_year, pattern_month_aug),
      allow_truncation=truncated >= 3
    )
  pattern_year_aug
}

#' Generate a regular expression for ISO 8601 times
#' 
#' Fractional hours and minutes are not allowed in the generated regular
#' expressions even though they are allowed in the ISO 8601 standard.
#' 
#' Leap seconds occur at 23:59:60Z at the end of the month.  Given timezone
#' differences which could occur on different hours and at the end of any
#' 15-minute point, \code{allow_leap_seconds} only makes a change to allow for
#' 60 seconds in any minute.
#' 
#' @param truncated Should the date be allowed to be truncated?  An integer
#'   indicating the highest required precision (0=second is required, 1=minute, and
#'   2=hour).  A value of 3 will allow an empty string to match.
#' @param allow_fractional_hours,allow_fractional_minutes Not yet supported
#' @param allow_fractional_seconds Should fractional seconds be allowed?
#' @param fractional_digits How many digits are allowed in fractional time
#'   units?  \code{0} indicates that they are not allowed, and \code{Inf}
#'   indicates any number are allowed)
#' @param pattern_decimal_mark What regular expression patterns should be
#'   allowed for the decimal mark?  Comma and period are both allowed in the
#'   standard and by default.
#' @param timezone Should the timezone be optional (NA, default), required
#'   (TRUE), or omitted (FALSE)?
#' @param allow_leap_second Should leap seconds (61 seconds in a minute) be
#'   allowed?
#' @param require_T Require the "T" at the beginning of the time?  TRUE, FALSE,
#'   or NA (optional)
#' @param ... Passed to \code{pattern_ISO8601_timezone()}
#' @family ISO8601 patterns
#' @export
pattern_ISO8601_time <- function(truncated=0,
                                 allow_fractional_hours=FALSE,
                                 allow_fractional_minutes=FALSE,
                                 allow_fractional_seconds=TRUE,
                                 fractional_digits=Inf, pattern_decimal_mark=c("\\.", ","),
                                 timezone=NA, allow_leap_second=FALSE, require_T=NA,
                                 ...) {
  stopifnot(is.numeric(truncated))
  stopifnot(length(truncated) == 1)
  stopifnot(!is.na(truncated))
  stopifnot(truncated >= 0 & truncated <= 3)

  stopifnot(is.logical(allow_fractional_hours))
  stopifnot(length(allow_fractional_hours) == 1)
  stopifnot(isFALSE(allow_fractional_hours))
  stopifnot(is.logical(allow_fractional_minutes))
  stopifnot(length(allow_fractional_minutes) == 1)
  stopifnot(isFALSE(allow_fractional_minutes))
  stopifnot(is.logical(allow_fractional_seconds))
  stopifnot(length(allow_fractional_seconds) == 1)
  stopifnot(!is.na(allow_fractional_seconds))
  
  stopifnot(is.numeric(fractional_digits))
  stopifnot(length(fractional_digits) == 1)
  stopifnot(fractional_digits >= 0)
  
  pattern_decimal_mark <- match.arg(pattern_decimal_mark, several.ok=TRUE)
  if (length(pattern_decimal_mark) > 1) {
    pattern_decimal_mark <- paste0("[", paste0(pattern_decimal_mark, collapse=""), "]")
  }
  
  stopifnot(is.logical(timezone))
  stopifnot(length(timezone) == 1)

  stopifnot(is.logical(allow_leap_second))
  stopifnot(length(allow_leap_second) == 1)
  stopifnot(!is.na(allow_leap_second))
  
  stopifnot(is.logical(require_T))
  stopifnot(length(require_T) == 1)

  pattern_hour_number <- "([01][0-9]|2[0-3])"
  pattern_hour_t <- paste0("T", pattern_hour_number)
  if (is.na(require_T)) {
    pattern_hour <- paste0("T?", pattern_hour_number)
  } else if (require_T) {
    pattern_hour <- pattern_hour_t
  } else {
    pattern_hour <- pattern_hour_number
  }
  pattern_minute <- ":([0-5][0-9])"
  partial_pattern_second_whole <-
    if (allow_leap_second) {
      "[0-5][0-9]|60"
    } else {
      "[0-5][0-9]"
    }
  pattern_fraction <-
    if (is.infinite(fractional_digits)) {
      paste0(pattern_decimal_mark, "[0-9]+")
    } else {
      sprintf("%s[0-9]{1,%g}", pattern_decimal_mark, fractional_digits)
    }
  if (fractional_digits == 0) {
    pattern_fraction <- ""
  } else {
    # fractions are always optional
    pattern_fraction <- sprintf("(?:%s)?", pattern_fraction)
  }
  pattern_second <-
    if (allow_fractional_seconds) {
      sprintf(
        ":((?:%s)%s)",
        partial_pattern_second_whole,
        pattern_fraction
      )
    } else {
      sprintf(":(%s)", partial_pattern_second_whole)
    }
  
  # Put it together
  pattern_second_aug <- pattern_ISO8601_truncated_helper(pattern_second, allow_truncation=truncated >= 1)
  # To support the requirement of "T", pattern_minute_aug is not truncated by
  # itself.
  pattern_minute_aug <- paste0(pattern_minute, pattern_second_aug)
  # T is required at the beginning for hour-only times
  if (truncated >= 2) {
    pattern_hour_aug <-
      sprintf(
        "(?:%s|%s)",
        pattern_hour_t,
        paste0(pattern_hour, pattern_minute_aug)
      )
  } else {
    pattern_hour_aug <- paste0(pattern_hour, pattern_minute_aug)
  }
  if (!isFALSE(timezone)) {
    pattern_timezone <-
      pattern_ISO8601_truncated_helper(
        pattern_ISO8601_timezone(),
        allow_truncation=is.na(timezone)
      )
    ret <-
      pattern_ISO8601_truncated_helper(
        paste0(pattern_hour_aug, pattern_timezone),
        allow_truncation=truncated >= 3
      )
  } else {
    ret <-
      pattern_ISO8601_truncated_helper(
        paste0(pattern_hour, pattern_minute_aug),
        allow_truncation=truncated >= 3
      )
  }
  ret
}

#' Generate a regular expression for ISO 8601 timezones.
#'
#' @details Offsets are always multiples of 15 minutes, so if the minute part is
#'   provided, it must be 00, 15, 30, or 45.  The range allowed by this function
#'   is a bit wider than the current range as of the writing of this function.
#'   The allowed range is -14:45 to +14:45 while the actual widest range is
#'   currently -12:00 to +14:00.
#'   
#'   According to the ISO standard, zero must always be preceded by a + and
#'   never a negative.  And, "Z" is allowed.
#'
#' @family ISO8601 patterns
#' @references https://en.wikipedia.org/wiki/UTC_offset
#' @export
pattern_ISO8601_timezone <- function() {
  zulu <- "Z"
  zero <- "\\+00(?::00)?"
  zero_with_minutes <- "[\\+-]00:(?:15|30|45)"
  nonzero <- "[\\+-](?:0[1-9]|1[1-4])(?::(?:00|15|30|45))?"
  paste0(
    "(",
    zulu, "|",
    zero, "|",
    zero_with_minutes, "|",
    nonzero,
    ")"
  )
}

# Add optionality outside of the regular expression
pattern_ISO8601_truncated_helper <- function(x, allow_truncation) {
  ret <- x
  if (allow_truncation) {
    ret <- paste0("(?:", x, ")?")
  }
  ret
}

# A helper function to make the pattern bound the start and end of the string
make_full_pattern <- function(x) {
  sprintf("^%s$", x)
}
