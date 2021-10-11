test_that("VISITDY_transform and VISITDY_inverse", {
  expect_equal(
    VISITDY_transform(c(-1:1, -Inf, Inf, NA)),
    c(-1L, -1L, 0L, -.Machine$integer.max, .Machine$integer.max, NA_integer_)
  )
  expect_equal(
    VISITDY_inverse(c(-1:1, -Inf, Inf, NA)),
    c(-1L, 1L, 2L, -Inf, Inf, NA_integer_)
  )
})

test_that("VISITDY_breaks", {
  expect_equal(
    VISITDY_breaks(5)(-5:5),
    c(-4, -2, -1, 0, 1, 2, 4)
  )
  expect_equal(
    VISITDY_breaks(5)(-100:100),
    c(-100, -50, -1, 0, 1, 50, 100)
  )
})

test_that("VISITDY_minor_breaks", {
  expect_equal(
    VISITDY_minor_breaks(b=c(-100, -50, -1, 0, 1, 50, 100), limits=c(-110, 110), n=3),
    setdiff(seq(-110, 110, by=1), c(-100, -50, -1, 0, 1, 50, 100))
  )
})

test_that("scale_x_VISITDY", {
  expect_equal(
    scale_x_VISITDY(),
    ggplot2::scale_x_continuous(trans=VISITDY_trans)
  )
})
