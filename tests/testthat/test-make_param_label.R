test_that("make_param_label errors", {
  expect_error(
    make_param_label(spec=NA, param=NA, unit=NA, allow_missing_spec=FALSE),
    regexp="spec may not be NA"
  )
  expect_error(
    make_param_label(spec="A", param=NA, unit=NA),
    regexp="param may not be NA"
  )
  expect_error(
    make_param_label(spec="A", param="A", unit=NA, allow_missing_unit=FALSE),
    regexp="unit may not be NA"
  )
  
  expect_error(
    make_param_label(spec=NA, param=NA, unit=NA),
    regexp="param may not be NA"
  )
  expect_error(
    make_param_label(spec="A", param=NA, unit=NA),
    regexp="param may not be NA"
  )
  expect_error(
    make_param_label(spec=c("A", "B"), param="A", unit=NA),
    regexp="More than one parameter label created"
  )
})

test_that("make_param_label", {
  expect_equal(
    make_param_label(spec=NA, param="A", unit=NA),
    "A"
  )
  expect_equal(
    make_param_label(spec="B", param="A", unit=NA),
    "B A"
  )
  expect_equal(
    make_param_label(spec=NA, param="A", unit="C"),
    "A (C)"
  )
  expect_equal(
    make_param_label(spec="B", param="A", unit="C"),
    "B A (C)"
  )
  expect_equal(
    make_param_label(spec=c("B", "D"), param="A", unit="C", expect_single=FALSE),
    c("B A (C)", "D A (C)")
  )
  expect_equal(
    make_param_label(spec=c("B", "D"), param="A", unit="C", expect_single=FALSE),
    c("B A (C)", "D A (C)")
  )
})
