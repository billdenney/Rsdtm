context("c_to_n")

test_that("vector classes other than character work", {
  expect_equal(expect_warning(c_to_n(1)), 1)
  expect_equal(expect_warning(c_to_n(as.integer(1))), as.integer(1))
  expect_equal(c_to_n(factor(1)), c_to_n("1"))
  expect_equal(expect_warning(c_to_n(TRUE)), NA_real_)
  expect_equal(c_to_n(NA), NA_real_, info="No warning with all logical(NA)")
})

test_that("character works for basic character strings", {
  expect_equal(c_to_n("5"), 5)
  expect_equal(c_to_n(c("5", "6")), 5:6)
  expect_equal(c_to_n(c("5", "^")), c(5, NA))
  expect_equal(c_to_n(c("5", NA)), c(5, NA))
})

test_that("character works for all expected numeric strings", {
  expect_equal(
    c_to_n(
      c("5", "6.", ".7", "6.1", "0.8",
        "2e1", "3E2", "4e+3", "5E-2")
    ),
    c(5, 6, 0.7, 6.1, 0.8,
      20, 300, 4000, 0.05),
    info="Working, general number formats"
  )
  expect_equal(
    c_to_n(
      c(" 5", " 6.", " .7", "6.1 ", "0.8 ",
        " 2e1 ", " 3E2 ", " 4e+3 ", " 5E-2")
    ),
    c(5, 6, 0.7, 6.1, 0.8,
      20, 300, 4000, 0.05),
    info="Spaces are removed from the beginning or end."
  )
})

test_that("data.frame (including tibble) outputs work", {
  expect_equal(
    c_to_n(data.frame(A=1)),
    data.frame(A=1),
    info="data.frames are unmodified, if no modification is required."
  )
  expect_equal(
    c_to_n(data.frame(STRESC=1)),
    data.frame(STRESC=1),
    info="data.frames are unmodified, if no columns matching the expected column regex are detected (even for close cousins)."
  )
  expect_equal(
    expect_warning(
      expect_message(
        c_to_n(data.frame(OOSTRESC=1)),
        regexp="Converting column OOSTRESC to OOSTRESN"
      ),
      regexp="is generally not called on a numeric vector"
    ),
    data.frame(OOSTRESC=1, OOSTRESN=1),
    info="data.frames are modified correctly, numeric input"
  )
  expect_equal(
    expect_message(
      c_to_n(data.frame(OOSTRESC="1")),
      regexp="Converting column OOSTRESC to OOSTRESN"
    ),
    data.frame(OOSTRESC="1", OOSTRESN=1),
    info="data.frames are modified correctly, factor input"
  )
  expect_equal(
    expect_message(
      c_to_n(data.frame(OOSTRESC="1")),
      regexp="Converting column OOSTRESC to OOSTRESN"
    ),
    data.frame(OOSTRESC="1", OOSTRESN=1),
    info="data.frames are modified correctly, character input"
  )
  expect_equal(
    expect_message(
      c_to_n(tibble(OOSTRESC="1")),
      regexp="Converting column OOSTRESC to OOSTRESN"
    ),
    tibble(OOSTRESC="1", OOSTRESN=1),
    info="tibbles are modified correctly, character input"
  )
  expect_equal(
    expect_message(
      c_to_n(tibble(OOSTRESC="1", OOSTRESN=2)),
      regexp="The following numeric columns already exist, not generating from the character equivalent: OOSTRESN"
    ),
    tibble(OOSTRESC="1", OOSTRESN=2),
    info="columns are not replaced"
  )
})