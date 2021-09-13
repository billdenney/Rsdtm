test_that("standardize_sdtm_id", {
  # Column order is standardized
  expect_equal(
    standardize_sdtm_id(data=data.frame(STUDYID="A", SUBJID="B", USUBJID="C")),
    data.frame(STUDYID="A", USUBJID="C", SUBJID="B")
  )
  # STUDYID is added
  expect_equal(
    standardize_sdtm_id(data=data.frame(SUBJID="B", USUBJID="C"), studyid="A"),
    data.frame(STUDYID="A", USUBJID="C", SUBJID="B")
  )
  # SUBJID is added
  expect_equal(
    standardize_sdtm_id(data=data.frame(USUBJID="A-C"), studyid="A"),
    data.frame(STUDYID="A", USUBJID="A-C", SUBJID="C")
  )
  # USUBJID is added
  expect_equal(
    standardize_sdtm_id(data=data.frame(SUBJID="A-C"), studyid="B"),
    data.frame(STUDYID="B", USUBJID="B-A-C", SUBJID="A-C")
  )
})

test_that("standardize_sdtm_id id addition", {
  expect_equal(
    standardize_sdtm_id(
      data=data.frame(RANDID="foo"), studyid="B",
      id=data.frame(RANDID="foo", SUBJID="C")
    ),
    data.frame(STUDYID="B", USUBJID="B-C", SUBJID="C")
  )
  expect_error(
    standardize_sdtm_id(
      data=data.frame(RANDID="foo"), studyid="B",
      id=data.frame(RANDID="foo", bar=c("bar", "baz"), SUBJID="C")
    ),
    regexp="Likely merge error, different number of rows in output than input"
  )
})

test_that("standardize_sdtm_id alternate paths", {
  expect_error(
    standardize_sdtm_id(data=data.frame(STUDYID="A", SUBJID=NA_character_, USUBJID="C")),
    regexp="NA not allowed in SUBJID"
  )
  expect_error(
    standardize_sdtm_id(data=data.frame(STUDYID="A", SUBJID="B", USUBJID=NA_character_)),
    regexp="NA not allowed in USUBJID"
  )
  expect_equal(
    standardize_sdtm_id(data=data.frame(STUDYID="A", SUBJID=NA_character_, USUBJID="C"), allow_missing_id=TRUE),
    data.frame(STUDYID="A", USUBJID="C", SUBJID=NA_character_)
  )
  expect_equal(
    standardize_sdtm_id(data=data.frame(STUDYID="A", SUBJID="B", USUBJID=NA_character_), allow_missing_id=TRUE),
    data.frame(STUDYID="A", USUBJID=NA_character_, SUBJID="B")
  )
  expect_equal(
    standardize_sdtm_id(data=data.frame(STUDYID="A", SUBJID="B", USUBJID=NA_character_), allow_missing_id=TRUE),
    data.frame(STUDYID="A", USUBJID=NA_character_, SUBJID=c("B"))
  )
  # Do not create "[STUDYID]-NA" values for USUBJID
  expect_equal(
    standardize_sdtm_id(data=data.frame(STUDYID="A", SUBJID=c("B", NA_character_)), allow_missing_id=TRUE),
    data.frame(STUDYID="A", USUBJID=c("A-B", NA_character_), SUBJID=c("B", NA_character_))
  )
})

test_that("standardize_sdtm_id errors", {
  expect_error(
    standardize_sdtm_id(data=data.frame(SUBJID="B", USUBJID="C"), sep_usubjid=factor("A")),
    regexp="'sep_usubjid' must be a character"
  )
  expect_error(
    standardize_sdtm_id(data=data.frame(SUBJID="B", USUBJID="C"), sep_usubjid=NA_character_),
    regexp="'sep_usubjid' must not be NA"
  )
  expect_error(
    standardize_sdtm_id(data=data.frame(SUBJID="B", USUBJID="C"), sep_usubjid=c("A", "B")),
    regexp="'sep_usubjid' must be a scalar"
  )
  expect_error(
    standardize_sdtm_id(data=data.frame(SUBJID="B", USUBJID="C")),
    regexp='argument "studyid" is missing, with no default'
  )
  expect_error(
    standardize_sdtm_id(data=data.frame(SUBJID="B", USUBJID="C"), studyid=factor("A")),
    regexp="'studyid' must be a character"
  )
  expect_error(
    standardize_sdtm_id(data=data.frame(SUBJID="B", USUBJID="C"), studyid=NA_character_),
    regexp="'studyid' must not be NA"
  )
  expect_error(
    standardize_sdtm_id(data=data.frame(SUBJID="B", USUBJID="C"), studyid=c("A", "B")),
    regexp="'studyid' must be a scalar"
  )
  expect_error(
    standardize_sdtm_id(data=data.frame(STUDYID=NA_character_, SUBJID="B", USUBJID="C")),
    regexp="NA not allowed in STUDYID"
  )
})
