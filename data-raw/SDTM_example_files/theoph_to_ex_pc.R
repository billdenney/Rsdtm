library(dplyr)
library(lubridate)
library(hms)
library(stringr)
library(readr)

Theoph <- datasets::Theoph

sdtm_example_theoph_pc <-
  Theoph %>%
  mutate(
    STUDYID = "S-CDSK-01",
    DOMAIN = "PC",
    SUBJID = str_pad(Subject, width = 3, side = "left", pad = "0"),
    USUBJID = paste0("CDISC01.", SUBJID),
    PCTESTCD = "THEOPH",
    PCTEST = "Theophylline concentration",
    PCTPTNUM = case_when(
      Time %in% c(
        0.57, 0.6, 0.52, 0.58, 0.5,
        0.63, 0.77
      ) ~ 0.5,
      Time %in% c(0.25, 0.27, 0.35, 0.3) ~ 0.25,
      Time %in% c(
        3.48, 3.82, 3.5, 3.62, 3.57, 3.53,
        3.55, 3.6, 3.52
      ) ~ 3.5,
      Time %in% 24.65 ~ 24,
      TRUE ~ round(Time)
    ),
    PCTPT = case_when(
      PCTPTNUM %in% 0 ~ "Pre-Dose",
      PCTPTNUM %in% c(0.25, 0.5, 1) ~
      paste(PCTPTNUM, "Hour Post-Dose"),
      TRUE ~ paste(PCTPTNUM, "Hours Post-Dose")
    ),
    PCORRES = as.character(conc),
    PCORRESU = "mg/L", # Units from '?datasets::Theoph'
    PCSTRESC = PCORRES,
    PCSTRESN = conc,
    PCSTRESU = PCORRESU,
    PCDT = case_when(
      PCTPTNUM %in% 24 ~ as.POSIXct("2003-04-30"),
      TRUE ~ as.POSIXct("2003-04-29")
    ),
    TIMEN = as.numeric(round(Time * 60)),
    TIME_MIN = difftime(as.POSIXct("08:00", format = "%H:%M"), as.POSIXct("00:00", format = "%H:%M"), units = "min"),
    TIME_TOTAL = case_when(
      PCTPTNUM %in% 0 ~ as_hms("07:45:00"),
      PCTPTNUM %in% 24 ~ as_hms("08:00:00"),
      TRUE ~ as_hms(TIMEN + TIME_MIN)
    ),
    DTC = as.POSIXct(paste0(PCDT, "T", TIME_TOTAL), format = "%Y-%m-%dT%H:%M"),
    PCDTC = format_ISO8601(DTC)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    PCSEQ = row_number()
  ) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, SUBJID, USUBJID, PCSEQ, PCTESTCD, PCTEST, PCORRES, PCORRESU,
    PCSTRESC, PCSTRESN, PCSTRESU, PCDTC, PCTPT, PCTPTNUM
  )

sdtm_example_theoph_ex <-
  Theoph %>%
  select(Subject, Dose) %>%
  unique() %>%
  mutate(
    STUDYID = "S-CDSK-01",
    DOMAIN = "EX",
    SUBJID = str_pad(Subject, width = 3, side = "left", pad = "0"),
    USUBJID = paste0("CDISC01.", SUBJID),
    EXTRT = "THEOPHYLLINE",
    EXROUTE = "ORAL", # Taken from '?datasets::Theoph'
    EXDOSE = Dose,
    EXDOSU = "mg/kg", # Taken from '?datasets::Theoph'
    DTC = as.POSIXct(paste0("2003-04-29", "T", "08:00"), format = "%Y-%m-%dT%H:%M"),
    EXSTDTC = format_ISO8601(DTC)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    EXSEQ = row_number()
  ) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, SUBJID, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXROUTE, EXSTDTC)

usethis::use_data(sdtm_example_theoph_pc)
usethis::use_data(sdtm_example_theoph_ex)
