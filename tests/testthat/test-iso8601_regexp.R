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
  expect_true(
    is_ISO8601_calendar_date("2020-11-01")
  )
  expect_equal(
    is_ISO8601_calendar_date(
      x=c("2020-11-01", "2020-11", "2020", "", "100"),
      truncated=0
    ),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_equal(
    is_ISO8601_calendar_date(
      x=c("2020-11-01", "2020-11", "2020", "", "100"),
      truncated=1
    ),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_equal(
    is_ISO8601_calendar_date(
      x=c("2020-11-01", "2020-11", "2020", "", "100"),
      truncated=2
    ),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    is_ISO8601_calendar_date(
      x=c("2020-11-01", "2020-11", "2020", "", "100"),
      truncated=3
    ),
    c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )

  ## allow_before_year_1583
  expect_equal(
    is_ISO8601_calendar_date(
      x=c("2020-11-01", "1582-01-01")
    ),
    c(TRUE, FALSE)
  )
  expect_equal(
    is_ISO8601_calendar_date(
      x=c("2020-11-01", "1582-01-01"),
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
  expect_match(
    c(
      "00:00:00+00",
      "00:00:00Z",
      "00:00:00+14:00",
      "00:00:00-14:00"
    ),
    make_full_pattern(pattern_ISO8601_time())
  )
  expect_no_match(
    c(
      "00:00:00+15:00",
      "00:00:00-15:00"
    ),
    make_full_pattern(pattern_ISO8601_time())
  )
  
  # truncated
  expect_equal(
    is_ISO8601_time(
      x=c("00:00:00", "00:00", "00", "", "0"),
      truncated=0
    ),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_equal(
    is_ISO8601_time(
      x=c("00:00:00", "00:00", "00", "", "0"),
      truncated=1
    ),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_equal(
    is_ISO8601_time(
      x=c("00:00:00", "00:00", "00", "", "0"),
      truncated=2
    ),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    is_ISO8601_time(
      x=c("00:00:00", "00:00", "00", "", "0"),
      truncated=3
    ),
    c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )

  # Timezone requirement
  expect_match(
    c(
      "00:00:00",
      "00:00:00Z"
    ),
    ## Default is optional timezone
    make_full_pattern(pattern_ISO8601_time())
  )
  ## No timezone
  expect_match(
    "00:00:00",
    make_full_pattern(pattern_ISO8601_time(timezone=FALSE))
  )
  expect_no_match(
    "00:00:00Z",
    make_full_pattern(pattern_ISO8601_time(timezone=FALSE))
  )
  ## timezone required
  expect_no_match(
    "00:00:00",
    make_full_pattern(pattern_ISO8601_time(timezone=TRUE))
  )
  expect_match(
    "00:00:00Z",
    make_full_pattern(pattern_ISO8601_time(timezone=TRUE))
  )
  
  # Fractional seconds
  ## Allowed by default
  expect_match(
    "00:00:00.123456789",
    make_full_pattern(pattern_ISO8601_time())
  )
  ## Alternate separator
  expect_match(
    "00:00:00,123456789",
    make_full_pattern(pattern_ISO8601_time())
  )
  ## Allowed
  expect_match(
    "00:00:00.123456789",
    make_full_pattern(pattern_ISO8601_time(allow_fractional_seconds=TRUE))
  )
  ## Not allowed
  expect_no_match(
    "00:00:00.123456789",
    make_full_pattern(pattern_ISO8601_time(allow_fractional_seconds=FALSE))
  )
  
  # pattern_decimal_mark
  expect_match(
    "00:00:00.123456789",
    make_full_pattern(pattern_ISO8601_time(pattern_decimal_mark="\\."))
  )
  expect_no_match(
    "00:00:00,123456789",
    make_full_pattern(pattern_ISO8601_time(pattern_decimal_mark="\\."))
  )
  # allow_leap_second
  expect_no_match(
    # Default is no match
    "00:00:60",
    make_full_pattern(pattern_ISO8601_time())
  )
  expect_match(
    "00:00:60",
    make_full_pattern(pattern_ISO8601_time(allow_leap_second=TRUE))
  )
  expect_no_match(
    "00:00:60",
    make_full_pattern(pattern_ISO8601_time(allow_leap_second=FALSE))
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
