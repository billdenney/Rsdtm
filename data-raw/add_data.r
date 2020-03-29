# Save the domain specification files into package
library(rio)
setwd('C:/Users/dengj25/Pfizer/AMP/Rsdtm/Branch2/Rsdtm/data-raw/SDTMIG_3.3/Domain_Specifications')
domain_spec_filnames <-
  list.files(pattern=".csv$")
names(domain_spec_filnames) <- substr(domain_spec_filnames, 1, 2)
domain_specs <-
  lapply(
    X=domain_spec_filnames,
    FUN=import
  )

domain_spec_list <- list(SDTMIG3.3=list(CO=domain_specs$CO,
                                        DM=domain_specs$DM,
                                        SE=domain_specs$SE,
                                        SM=domain_specs$SM,
                                        SV=domain_specs$SV))

# Save the SDTM Terminology file into package
library(readxl)
library(httr)
url1<-'https://evs.nci.nih.gov/ftp1/CDISC/SDTM/SDTM%20Terminology.xls'
GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
terminology <- read_excel(tf, 2L)

setwd('C:/Users/dengj25/Pfizer/AMP/Rsdtm/Branch2/Rsdtm/R')

# Adding datasets to package
usethis::use_data(domain_spec_list)
usethis::use_data(terminology)