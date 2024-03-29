% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iso8601_regexp.R
\name{pattern_ISO8601_calendar_year}
\alias{pattern_ISO8601_calendar_year}
\title{Generate a regular expression matching an ISO8601 calendar year}
\usage{
pattern_ISO8601_calendar_year(allow_before_year_1583 = FALSE)
}
\arguments{
\item{allow_before_year_1583}{Should years between 0 and 1582 be allowed
(they are only allowed in ISO 8601 with mutual agreement)}
}
\description{
Generate a regular expression matching an ISO8601 calendar year
}
\details{
Sign on the year (+ or -) is not supported (therefore years before
  0000 are not supported). Years after 9999 are not supported.
}
\references{
https://en.wikipedia.org/wiki/ISO_8601
}
\seealso{
Other ISO8601 patterns: 
\code{\link{pattern_ISO8601_any_datetime}()},
\code{\link{pattern_ISO8601_any_date}()},
\code{\link{pattern_ISO8601_calendar_datetime}()},
\code{\link{pattern_ISO8601_calendar_date}()},
\code{\link{pattern_ISO8601_ordinal_datetime}()},
\code{\link{pattern_ISO8601_ordinal_date}()},
\code{\link{pattern_ISO8601_timezone}()},
\code{\link{pattern_ISO8601_time}()},
\code{\link{pattern_ISO8601_week_datetime}()},
\code{\link{pattern_ISO8601_week_date}()}

Other Date management and conversion: 
\code{\link{dateany_to_date}()},
\code{\link{generate_dtc}()},
\code{\link{is_ISO8601_calendar_datetime}()},
\code{\link{is_ISO8601_calendar_date}()},
\code{\link{is_ISO8601_ordinal_datetime}()},
\code{\link{is_ISO8601_timezone}()},
\code{\link{is_ISO8601_time}()},
\code{\link{is_ISO8601_week_datetime}()},
\code{\link{make_dy}()},
\code{\link{pattern_ISO8601_any_datetime}()},
\code{\link{pattern_ISO8601_any_date}()},
\code{\link{pattern_ISO8601_calendar_datetime}()},
\code{\link{pattern_ISO8601_calendar_date}()},
\code{\link{pattern_ISO8601_ordinal_datetime}()},
\code{\link{pattern_ISO8601_ordinal_date}()},
\code{\link{pattern_ISO8601_timezone}()},
\code{\link{pattern_ISO8601_time}()},
\code{\link{pattern_ISO8601_week_datetime}()},
\code{\link{pattern_ISO8601_week_date}()},
\code{\link{sdtm_dtc_to_datetime}()},
\code{\link{sdtm_first_dose}()},
\code{\link{sdtm_time_actual}()}
}
\concept{Date management and conversion}
\concept{ISO8601 patterns}
