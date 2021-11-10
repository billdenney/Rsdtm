# pattern_ISO8601_calendar_datetime ####

## pattern_ISO8601_calendar_datetime expected errors ####

test_that("pattern_ISO8601_calendar_datetime", {
  datetimes_to_test <-
    c(
      "2020-11-01T23:59:59Z",
      "2020-11-01T23:59:59",
      "2020-11-01T23:59Z",
      "2020-11-01T23:59",
      "2020-11-01T23Z",
      "2020-11-01T23",
      "2020-11-01Z",
      "2020-11-01",
      "2020-11-Z",
      "2020-11-",
      "2020-11Z",
      "2020-11",
      "2020-Z",
      "2020-",
      "2020Z",
      "2020",
      "200",
      ""
    )
  ## default
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test),
    c(
      TRUE,  # ymdThms tz
      TRUE,  # ymdThms
      FALSE, # ymdThm tz
      FALSE, # ymdThm
      FALSE, # ymdTh tz
      FALSE, # ymdTh
      FALSE, # ymd tz (always invalid)
      FALSE, # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      FALSE, # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      FALSE, # y
      FALSE, # a 3-digit number (always invalid)
      FALSE  # an empty string
    )
  )
  ## truncated
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test, truncated=1),
    c(
      TRUE,  # ymdThms tz
      TRUE,  # ymdThms
      TRUE,  # ymdThm tz
      TRUE,  # ymdThm
      FALSE, # ymdTh tz
      FALSE, # ymdTh
      FALSE, # ymd tz (always invalid)
      FALSE, # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      FALSE, # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      FALSE, # y
      FALSE, # a 3-digit number (always invalid)
      FALSE  # an empty string
    )
  )
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test, truncated=2),
    c(
      TRUE,  # ymdThms tz
      TRUE,  # ymdThms
      TRUE,  # ymdThm tz
      TRUE,  # ymdThm
      TRUE,  # ymdTh tz
      TRUE,  # ymdTh
      FALSE, # ymd tz (always invalid)
      FALSE, # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      FALSE, # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      FALSE, # y
      FALSE, # a 3-digit number (always invalid)
      FALSE  # an empty string
    )
  )
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test, truncated=3),
    c(
      TRUE,  # ymdThms tz
      TRUE,  # ymdThms
      TRUE,  # ymdThm tz
      TRUE,  # ymdThm
      TRUE,  # ymdTh tz
      TRUE,  # ymdTh
      FALSE, # ymd tz (always invalid)
      TRUE,  # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      FALSE, # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      FALSE, # y
      FALSE, # a 3-digit number (always invalid)
      FALSE  # an empty string
    )
  )
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test, truncated=4),
    c(
      TRUE,  # ymdThms tz
      TRUE,  # ymdThms
      TRUE,  # ymdThm tz
      TRUE,  # ymdThm
      TRUE,  # ymdTh tz
      TRUE,  # ymdTh
      FALSE, # ymd tz (always invalid)
      TRUE,  # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      TRUE,  # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      FALSE, # y
      FALSE, # a 3-digit number (always invalid)
      FALSE  # an empty string
    )
  )
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test, truncated=5),
    c(
      TRUE,  # ymdThms tz
      TRUE,  # ymdThms
      TRUE,  # ymdThm tz
      TRUE,  # ymdThm
      TRUE,  # ymdTh tz
      TRUE,  # ymdTh
      FALSE, # ymd tz (always invalid)
      TRUE,  # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      TRUE,  # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      TRUE,  # y
      FALSE, # a 3-digit number (always invalid)
      FALSE  # an empty string
    )
  )
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test, truncated=6),
    c(
      TRUE,  # ymdThms tz
      TRUE,  # ymdThms
      TRUE,  # ymdThm tz
      TRUE,  # ymdThm
      TRUE,  # ymdTh tz
      TRUE,  # ymdTh
      FALSE, # ymd tz (always invalid)
      TRUE,  # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      TRUE,  # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      TRUE,  # y
      FALSE, # a 3-digit number (always invalid)
      TRUE   # an empty string
    )
  )
  ## timezone
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test, timezone=TRUE),
    c(
      TRUE,  # ymdThms tz
      FALSE,  # ymdThms
      FALSE, # ymdThm tz
      FALSE, # ymdThm
      FALSE, # ymdTh tz
      FALSE, # ymdTh
      FALSE, # ymd tz (always invalid)
      FALSE, # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      FALSE, # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      FALSE, # y
      FALSE, # a 3-digit number (always invalid)
      FALSE  # an empty string
    )
  )
  
  expect_equal(
    is_ISO8601_calendar_datetime(x=datetimes_to_test, timezone=FALSE),
    c(
      FALSE, # ymdThms tz
      TRUE,  # ymdThms
      FALSE, # ymdThm tz
      FALSE, # ymdThm
      FALSE, # ymdTh tz
      FALSE, # ymdTh
      FALSE, # ymd tz (always invalid)
      FALSE, # ymd
      FALSE, # ym- tz (always invalid)
      FALSE, # ym- (always invalid)
      FALSE, # ym tz (always invalid)
      FALSE, # ym
      FALSE, # y- tz (always invalid)
      FALSE, # y- (always invalid)
      FALSE, # y tz (always invalid)
      FALSE, # y
      FALSE, # a 3-digit number (always invalid)
      FALSE  # an empty string
    )
  )
})

test_that("pattern_ISO8601_calendar_datetime expected errors", {
  expect_error(pattern_ISO8601_calendar_datetime(truncated="A"))
  expect_error(pattern_ISO8601_calendar_datetime(truncated=1:2))
  expect_error(pattern_ISO8601_calendar_datetime(truncated=-1))
  expect_error(pattern_ISO8601_calendar_datetime(truncated=7))
})

# pattern_ISO8601_calendar_date ####

test_that("pattern_ISO8601_calendar_date", {
  valid_date_ymd <- "2020-11-01"
  valid_date_ym <- "2020-11"
  valid_date_y <- "2020"
  valid_date_none <- ""
  invalid_date_ymd <- c("2020-11-00", "2020-11-32", "2020-11-40")
  invalid_date_ym <- c("2020-00", "2020-13", "2020-20")
  invalid_date_y <- "100"
  invalid_date_general <- "A"
  expect_true(
    all(is_ISO8601_calendar_date(c(
      valid_date_ymd
    )))
  )
  expect_false(
    any(is_ISO8601_calendar_date(c(
      invalid_date_ymd, invalid_date_ym, invalid_date_y, invalid_date_general,
      valid_date_ym, valid_date_y, valid_date_none
    )))
  )
  expect_true(
    all(is_ISO8601_calendar_date(
      c(
        valid_date_ymd
      ),
      truncated=0
    ))
  )
  expect_false(
    any(is_ISO8601_calendar_date(
      c(
        invalid_date_ymd, invalid_date_ym, invalid_date_y, invalid_date_general,
        valid_date_ym, valid_date_y, valid_date_none
      ),
      truncated=0
    ))
  )
  expect_true(
    all(is_ISO8601_calendar_date(
      c(
        valid_date_ymd, valid_date_ym
      ),
      truncated=1
    ))
  )
  expect_false(
    any(is_ISO8601_calendar_date(
      c(
        invalid_date_ymd, invalid_date_ym, invalid_date_y, invalid_date_general,
        valid_date_y, valid_date_none
      ),
      truncated=1
    ))
  )
  expect_true(
    all(is_ISO8601_calendar_date(
      c(
        valid_date_ymd, valid_date_ym, valid_date_y
      ),
      truncated=2
    ))
  )
  expect_false(
    any(is_ISO8601_calendar_date(
      c(
        invalid_date_ymd, invalid_date_ym, invalid_date_y, invalid_date_general,
        valid_date_none
      ),
      truncated=2
    ))
  )
  expect_true(
    all(is_ISO8601_calendar_date(
      c(
        valid_date_ymd, valid_date_ym, valid_date_y, valid_date_none
      ),
      truncated=3
    ))
  )
  expect_false(
    any(is_ISO8601_calendar_date(
      c(
        invalid_date_ymd, invalid_date_ym, invalid_date_y, invalid_date_general
      ),
      truncated=3
    ))
  )
    expect_equal(
    is_ISO8601_calendar_date(
      x=c(valid_date_ymd, "1582-01-01"),
      allow_before_year_1583=TRUE
    ),
    c(TRUE, TRUE)
  )
})

## pattern_ISO8601_calendar_date expected errors ####

test_that("pattern_ISO8601_calendar_date expected errors", {
  expect_error(pattern_ISO8601_calendar_date(truncated="A"))
  expect_error(pattern_ISO8601_calendar_date(truncated=1:2))
  expect_error(pattern_ISO8601_calendar_date(truncated=4))
  expect_error(pattern_ISO8601_calendar_date(truncated=-1))
  
  expect_error(pattern_ISO8601_calendar_date(allow_before_year_1583="A"))
  expect_error(pattern_ISO8601_calendar_date(allow_before_year_1583=NA))
  
  expect_error(pattern_ISO8601_calendar_date(time_pattern=factor("A")))
  expect_error(pattern_ISO8601_calendar_date(time_pattern=c("A", "B")))
  expect_error(pattern_ISO8601_calendar_date(time_pattern=NA_character_))
})

# pattern_ISO8601_time ####

test_that("pattern_ISO8601_time", {
  valid_time_timezone <-
    c(
      "00:00:00+00",
      "00:00:00Z",
      "00:00:00+14:00",
      "00:00:00-14:00"
    )
  invalid_time_timezone <-
    c(
      "00:00:00+15:00",
      "00:00:00-15:00",
      "00:00:00A"
    )
  expect_true(
    all(is_ISO8601_time(valid_time_timezone))
  )
  expect_true(
    !any(is_ISO8601_time(invalid_time_timezone))
  )
  
  # truncated
  valid_time_hms <-
    c(
      "T00:00:00",
      "00:00:00"
    )
  valid_time_hm <-
    c(
      "T00:00",
      "00:00"
    )
  # T is required when no minute or second is given
  valid_time_h <- "T00"
  valid_time_none <- ""
  invalid_time_hms <-
    c(
      "T00:00:61",
      "00:00:61",
      "T00:60:00",
      "00:60:00",
      "T24:00:00",
      "24:00:00",
      "T30:00:00",
      "30:00:00"
    )
  invalid_time_hm <-
    c(
      "T00:60",
      "00:60",
      "T24:00",
      "24:00",
      "T30:00",
      "30:00"
    )
  invalid_time_h <-
    c(
      "T24",
      "24",
      "T30",
      "30",
      # hour by itself must have a "T"
      "00",
      "0"
    )
  
  expect_true(
    all(is_ISO8601_time(x=valid_time_hms, truncated=0))
  )
  expect_true(
    !any(is_ISO8601_time(
      x=c(
        invalid_time_hms, invalid_time_hm, invalid_time_h,
        valid_time_hm, valid_time_h, valid_time_none
      ),
      truncated=0
    ))
  )
  expect_true(
    all(is_ISO8601_time(
      x=c(valid_time_hms, valid_time_hm),
      truncated=1
    ))
  )
  expect_true(
    !any(is_ISO8601_time(
      x=c(
        invalid_time_hms, invalid_time_hm, invalid_time_h,
        valid_time_h, valid_time_none
      ),
      truncated=1
    ))
  )
  expect_true(
    all(is_ISO8601_time(
      x=c(valid_time_hms, valid_time_hm, valid_time_h),
      truncated=2
    ))
  )
  expect_true(
    !any(is_ISO8601_time(
      x=c(
        invalid_time_hms, invalid_time_hm, invalid_time_h,
        valid_time_none
      ),
      truncated=2
    ))
  )
  expect_true(
    all(is_ISO8601_time(
      x=c(valid_time_hms, valid_time_hm, valid_time_h, valid_time_none),
      truncated=3
    ))
  )
  expect_true(
    !any(is_ISO8601_time(
      x=c(
        invalid_time_hms, invalid_time_hm, invalid_time_h
      ),
      truncated=3
    ))
  )

  # Timezone requirement
  valid_hms_notz <- "00:00:00"
  valid_hms_tz <- "00:00:00Z"
  expect_true(
    ## Default is optional timezone
    all(is_ISO8601_time(c(valid_hms_notz, valid_hms_tz)))
  )
  ## No timezone
  expect_true(
    is_ISO8601_time(valid_hms_notz, timezone=FALSE)
  )
  expect_false(
    is_ISO8601_time(valid_hms_tz, timezone=FALSE)
  )
  ## timezone required
  expect_false(
    is_ISO8601_time(valid_hms_notz, timezone=TRUE)
  )
  expect_true(
    is_ISO8601_time(valid_hms_tz, timezone=TRUE)
  )
  
  # Fractional seconds
  valid_hmsf_period <- "00:00:00.123456789"
  valid_hmsf_comma <- "00:00:00,123456789"
  valid_hmsf_either <- c(valid_hmsf_period, valid_hmsf_comma)
  ## Allowed by default with either separator
  expect_true(
    all(is_ISO8601_time(valid_hmsf_either))
  )
  ## Allowed
  expect_true(
    all(is_ISO8601_time(valid_hmsf_either, allow_fractional_seconds=TRUE))
  )
  ## Not allowed
  expect_false(
    any(is_ISO8601_time(valid_hmsf_either, allow_fractional_seconds=FALSE))
  )
  
  # pattern_decimal_mark
  expect_true(
    is_ISO8601_time(valid_hmsf_period, pattern_decimal_mark="\\.")
  )
  expect_false(
    is_ISO8601_time(valid_hmsf_comma, pattern_decimal_mark="\\.")
  )
  # allow_leap_second
  valid_leap_second <- "00:00:60"
  invalid_leap_second <- "00:00:61"
  expect_false(
    # Default is no match
    any(is_ISO8601_time(c(valid_leap_second, invalid_leap_second)))
  )
  expect_true(
    is_ISO8601_time(valid_leap_second, allow_leap_second=TRUE)
  )
  expect_false(
    is_ISO8601_time(invalid_leap_second, allow_leap_second=TRUE)
  )
  expect_false(
    is_ISO8601_time(valid_leap_second, allow_leap_second=FALSE)
  )
})

## pattern_ISO8601_time expected errors ####

test_that("pattern_ISO8601_time required/expected errors", {
  expect_error(pattern_ISO8601_time(truncated=-1))
  expect_error(pattern_ISO8601_time(truncated="A"))
  expect_error(pattern_ISO8601_time(truncated=4))
  expect_error(pattern_ISO8601_time(truncated=c(1, 2)))
  expect_error(pattern_ISO8601_time(truncated=NA_real_))
  
  expect_error(pattern_ISO8601_time(allow_fractional_hours="A"))
  expect_error(pattern_ISO8601_time(allow_fractional_hours=TRUE))
  expect_error(pattern_ISO8601_time(allow_fractional_hours=rep(FALSE, 2)))
  expect_error(pattern_ISO8601_time(allow_fractional_minutes="A"))
  expect_error(pattern_ISO8601_time(allow_fractional_minutes=TRUE))
  expect_error(pattern_ISO8601_time(allow_fractional_minutes=rep(FALSE, 2)))
  expect_error(pattern_ISO8601_time(allow_fractional_seconds="A"))
  expect_error(pattern_ISO8601_time(allow_fractional_seconds=NA))
  expect_error(pattern_ISO8601_time(allow_fractional_seconds=rep(FALSE, 2)))

  expect_error(pattern_ISO8601_time(fractional_digits="A"))
  expect_error(pattern_ISO8601_time(fractional_digits=rep(2, 2)))
  expect_error(pattern_ISO8601_time(fractional_digits=-1))

  expect_error(pattern_ISO8601_time(pattern_decimal_mark="x"))

  expect_error(pattern_ISO8601_time(timezone="A"))
  expect_error(pattern_ISO8601_time(timezone=rep(FALSE, 2)))
  
  expect_error(pattern_ISO8601_time(allow_leap_second="A"))
  expect_error(pattern_ISO8601_time(allow_leap_second=NA))
  expect_error(pattern_ISO8601_time(allow_leap_second=rep(FALSE, 2)))
  
  expect_error(pattern_ISO8601_time(require_T="A"))
  expect_error(pattern_ISO8601_time(require_T=rep(FALSE, 2)))
})

# pattern_ISO8601_timezone ####

test_that("pattern_ISO8601_timezone", {
  expect_match(
    c(
      "Z",
      "+00",
      "+00:00",
      # Two digits, in range
      "+14",
      "+04",
      "-04",
      "-14",
      # Zero with minutes works
      "-00:15", "+00:15",
      # Minute parts may only be 00, 15, 30, or 45
      "-01:15", "+14:15"
    ),
    make_full_pattern(pattern_ISO8601_timezone())
  )
  expect_no_match(
    c(
      # Zero is only positive by the standard
      "-00",
      "-00:00",
      # +-15 is out of range
      "+15",
      "-15",
      # One digit is invalid
      "+2",
      "-2",
      # Minutes other than 00, 15, 30, and 45 are invalid
      "-01:10"
    ),
    make_full_pattern(pattern_ISO8601_timezone())
  )
})
