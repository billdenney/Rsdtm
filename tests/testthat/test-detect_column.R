context("detect_column")

test_that("check if required columns exist", {
  expect_error(validate_required_column_names(data=data.frame(STUDYID="TEST",
                                                              DOMAIN="TEST",
                                                              USUBJID="TEST",
                                                              SUBJID="TEST",
                                                              SITEID="TEST",
                                                              SEX="TEST"),"DM"), 
               "The following required columns are missing: COUNTRY",
  info="Required column, COUNTRY, is missing, check error message")
  expect_silent(validate_required_column_names(data=data.frame(STUDYID="TEST",
                                                              DOMAIN="TEST",
                                                              USUBJID="TEST",
                                                              SUBJID="TEST",
                                                              SITEID="TEST",
                                                              SEX="TEST",
                                                              COUNTRY="TEST",
                                                              EXTRA="EXTRA"),"DM"))
  })

test_that("check if expected columns exist", {
  expect_warning(validate_expected_column_names(data=data.frame(RFSTDTC="TEST",
                                                              RFENDTC ="TEST",
                                                              RFXSTDTC="TEST",
                                                              RFXENDTC="TEST",
                                                              RFICDTC="TEST",
                                                              RFPENDTC="TEST",
                                                              DTHDTC="TEST",
                                                              DTHFL="TEST",
                                                              AGE=1,
                                                              AGEU="TEST",
                                                              RACE="TEST",
                                                              ARMCD="TEST",
                                                              ARM="TEST",
                                                              ACTARMCD="TEST",
                                                              ACTARM="TEST",
                                                              ARMNRS="TEST"),"DM"),
               "The following expected columns are missing: ACTARMUD",
               info="Expected column ACTARMUD is missing, check warning message")
  expect_silent(validate_expected_column_names(data=data.frame(RFSTDTC="TEST",
                                                                RFENDTC ="TEST",
                                                                RFXSTDTC="TEST",
                                                                RFXENDTC="TEST",
                                                                RFICDTC="TEST",
                                                                RFPENDTC="TEST",
                                                                DTHDTC="TEST",
                                                                DTHFL="TEST",
                                                                AGE=1,
                                                                AGEU="TEST",
                                                                RACE="TEST",
                                                                ARMCD="TEST",
                                                                ARM="TEST",
                                                                ACTARMCD="TEST",
                                                                ACTARM="TEST",
                                                                ARMNRS="TEST",
                                                                ACTARMUD="TEST",
                                                                EXTRA="EXTRA"),"DM"))
})

test_that("check if permitted columns exist", {
  expect_silent(validate_permitted_column_names(data=data.frame(INVID="TEST",
                                                               INVNAM="TEST",
                                                               BRTHDTC="TEST",
                                                               ETHNIC="TEST",
                                                               DMDTC="TEST"),"DM"))
  expect_silent(validate_permitted_column_names(data=data.frame(INVID="TEST",
                                                               INVNAM="TEST",
                                                               BRTHDTC="TEST",
                                                               ETHNIC="TEST",
                                                               DMDTC="TEST",
                                                               DMDY=1,
                                                               EXTRA="EXTRA"),"DM"))
})

test_that("check if extra columns exist", {
  expect_warning(validate_extra_column_names(data=data.frame(STUDYID="TEST",
                                                             EXTRA="EXTRA"),"DM"),
                 "The following extra columns do exist: EXTRA",
                 info="Extra column EXTRA exists, check warning message")
  expect_silent(validate_extra_column_names(data=data.frame(STUDYID="TEST"),"DM"))
})

test_that("check if columns have expected class (numeric or character) and convert if necessary", {
  expect_equal(
    expect_warning(validate_column_class(data=data.frame(STUDYID=1,AGE=1),"DM"),
                 "The following columns of class 'numeric' or 'logical' should be and have been converted to character: STUDYID"),
               data.frame(STUDYID="1",AGE=1,stringsAsFactors = F),
    info="The column STUDYID is numeric but should be character, check for warning message and conversion to character")
  expect_equal(
    expect_warning(validate_column_class(data=data.frame(STUDYID="1",AGE="1",stringsAsFactors = F),"DM"),
                   "The following columns of class 'character' should be and have been converted to numeric: AGE"),
    data.frame(STUDYID="1",AGE=1,stringsAsFactors = F),
    info="The column AGE is character but should be numeric, check for warning message and conversion to numeric")
  expect_error(validate_column_class(data=data.frame(STUDYID=c("1","2"),AGE=c(1,NA),stringsAsFactors = F),"DM"),
                   "The following columns in dataset have NA: AGE",
    info="The column AGE contains NA, check for error message")
})

test_that("check if columns have controlled terminology", {
  expect_error(validate_ctrl_terminology(data=data.frame(STUDYID="TEST",
                                                         SEX="TEST"),"DM"),
                 "The following columns has entries not consistent with controlled terminology: SEX",
                 info="Column SEX has entries not consistent with controlled terminology, check error message")
  expect_silent(validate_ctrl_terminology(data=data.frame(STUDYID="TEST",
                                                         SEX="UNDIFFERENTIATED"),"DM"))
})

test_that("check if columns are consistent with ISO format", {
  expect_error(validate_iso(data=data.frame(COUNTRY=c('AFG','AGF'),
                                            BRTHDTC=c('2009-05-19','1969-05-19'),
                                            RFSTDTC=c('2009-05-19','1969-05-19')),"DM"),
               "The following columns are not consistent with ISO 3166-1 Alpha-3 standards: COUNTRY",
               info="Column COUNTRY has entries not consistent with ISO 3166-1 Alpha-3 standards, check error message")
  expect_error(validate_iso(data=data.frame(COUNTRY=c('AFG','AFG'),
                                            BRTHDTC=c('2009-05-19','1969-5-19'),
                                            RFSTDTC=c('2009-05-19','1969-05-19')),"DM"),
               "The following columns are not consisent with ISO 8601: BRTHDTC",
               info="Column BRTHDTC has entries not consistent with ISO 8601, check error message")
})