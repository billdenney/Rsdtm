test_that("generate_dtc works", {
  expect_equal(
    generate_dtc(datetime="2020-05-01T01:02:03"),
    "2020-05-01T01:02:03",
    info="Datetimes provided are not changed"
  )
  expect_equal(
    generate_dtc(datetime="2020-05-01T01:02:03", date="2020-04-01"),
    "2020-05-01T01:02:03",
    info="Datetimes provided take precedence over dates"
  )
  expect_equal(
    generate_dtc(
      datetime=c(NA, "2020-05-01T01:02:03"),
      date=c("2020-04-01", NA)
    ),
    c("2020-04-01TUN:UN:UN", "2020-05-01T01:02:03")
  )
  expect_equal(
    generate_dtc(
      datetime=c(NA, "2020-05-01T01:02:03", NA),
      date=c("2020-04-01", NA, NA)
    ),
    c("2020-04-01TUN:UN:UN", "2020-05-01T01:02:03", NA)
  )
  expect_equal(
    generate_dtc(as.POSIXct("2020-05-01 01:02:03")),
    "2020-05-01T01:02:03",
    info="non-character `datetime` input is coerced to character"
  )
  expect_equal(
    generate_dtc(date=as.POSIXct("2020-05-01 01:02:03")),
    "2020-05-01TUN:UN:UN",
    info="non-character `date` input is coerced to character (and the time part is ignored)."
  )
  expect_equal(
    generate_dtc(date="2020-05-15", time="6:01"),
    "2020-05-15T06:01",
    info="times are zero-padded, when needed."
  )
  expect_equal(
    generate_dtc(date="2020-05-15", time="4:01", assume_24_hr_time=TRUE),
    generate_dtc(date="2020-05-15", time="04:01"),
    info="24-hour clock triggering only occurs when zero-padding is required and when assume_24_hr_time=FALSE"
  )
})

test_that("generate_dtc errors correctly", {
  expect_error(
    generate_dtc(),
    regexp="At least one of `datetime` or `date` must be given"
  )
  expect_error(
    generate_dtc(LETTERS[1:3], LETTERS[1:2]),
    regexp="`datetime` and `date` must be the same length"
  )
  expect_error(
    generate_dtc(date=LETTERS[1:3], time=LETTERS[1:2]),
    regexp="`date` and `time` must be the same length"
  )
  expect_error(
    generate_dtc(date="2020-05-15", time="4:01"),
    regexp="Some times appear to be very early in the morning (before 05 hours), please confirm that these times come from a 24-hour clock.  If they do, set `assume_24_hr_time=TRUE`",
    fixed=TRUE,
    info="Unusual times are caught"
  )
  expect_error(
    generate_dtc(date="foo"),
    regexp="Some output does not appear to be an ISO8601 datetime formatted with SDTM unknowns: fooTUN:UN:UN",
    fixed=TRUE,
    info="invalid formats are caught"
  )
})

test_that("dateany_to_date", {
  expect_error(
    dateany_to_date(c("2022-01-02", "2022-02-31", "")),
    "Not all input strings match date formats: ''"
  )
  expect_equal(
    dateany_to_date("2022-01-02"),
    as.Date("2022-01-02")
  )
  expect_equal(
    dateany_to_date("2022-01-02T03:04"),
    as.Date("2022-01-02")
  )
  expect_equal(
    dateany_to_date(as.Date("2022-01-02")),
    as.Date("2022-01-02")
  )
  expect_equal(
    dateany_to_date(as.POSIXct("2022-01-02T03:04", tryFormats = "%Y-%m-%dT%H:%M")),
    as.Date("2022-01-02")
  )
  expect_equal(
    dateany_to_date(c("2022-01-02", "2022-01-03")),
    as.Date(c("2022-01-02", "2022-01-03"))
  )
  expect_equal(
    dateany_to_date(c("2022-01-02", "2022-01-03", NA)),
    as.Date(c("2022-01-02", "2022-01-03", NA))
  )
  expect_error(
    dateany_to_date(5),
    "Invalid input class: numeric"
  )
  expect_error(
    dateany_to_date(c("2022-01-02", "2022-02-31", NA)),
    "NAs introduced in date conversion: '2022-02-31'"
  )
})

test_that("make_dy", {
  expect_equal(
    make_dy(c("2022-01-02", "2022-01-03", "2022-01-04"), "2022-01-03"),
    c(-1L, 1L, 2L)
  )
  expect_equal(
    make_dy(c("2022-01-02", "2022-01-03", "2022-01-04", NA), "2022-01-03"),
    c(-1L, 1L, 2L, NA_integer_)
  )
  expect_error(
    make_dy(c("2022-01-02", "2022-01-03", "2022-01-04", NA), c("2022-01-03", "2022-01-04"))
  )
  expect_equal(
    make_dy(c("2022-01-02", "2022-01-03", "2022-01-04", NA), rep(c("2022-01-03", "2022-01-04"), each = 2)),
    c(-1L, 1L, 1L, NA_integer_)
  )
})
