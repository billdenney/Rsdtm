# Load and save the SDTM terminology data set

library(tidyverse)
library(janitor)
library(assertr)

url <- "https://evs.nci.nih.gov/ftp1/CDISC/SDTM/SDTM%20Terminology.xls"
local_file <- file.path(tempdir(), basename(url))
download_result <- curl::curl_download(url=url, destfile=local_file, quiet=FALSE)

d_raw <- rio::import_list(local_file)
names(d_raw)
sheet_to_use <- names(d_raw)[startsWith(names(d_raw), "SDTM Terminology")]
if (length(sheet_to_use) != 1) {
  stop("Could not find correct sheet name")
} else {
  sdtm_terminology <-
    d_raw[[sheet_to_use]] %>%
    clean_names() %>%
    verify(is.na(codelist_code) | is.na(codelist_extensible_yes_no)) %>%
    verify(is.na(codelist_code) | (codelist_code != code)) %>%
    verify(
      !is.na(codelist_code) |
        (is.na(codelist_code) & codelist_name == cdisc_synonym_s)
    ) %>%
    #select(-codelist_name) %>%
    rename_with(
      .fn=gsub,
      pattern="^(CDISC|NCI)",
      replacement="\\U\\1",
      ignore.case=TRUE,
      perl=TRUE
    ) %>%
    rename(CDISC_synonym=CDISC_synonym_s)
  attr(sdtm_terminology, "version") <- sheet_to_use
  usethis::use_data(sdtm_terminology, compress="xz", overwrite=TRUE, version=3)
}
