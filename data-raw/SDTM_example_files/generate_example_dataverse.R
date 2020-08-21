library(devtools)
library(usethis)

# AE Domain
sdtm_example_dataverse_ae <-
  read.csv(file="Harvard directory/ae_raw.csv", stringsAsFactors=FALSE)
sdtm_example_dataverse_ae$AESPID <- as.character(sdtm_example_dataverse_ae$AESPID)
use_data(sdtm_example_dataverse_ae)

#DM Domain
sdtm_example_dataverse_dm <-
  read.csv(file="Harvard directory/dm.csv", stringsAsFactors=FALSE)
sdtm_example_dataverse_dm$SUBJID <- as.character(sdtm_example_dataverse_dm$SUBJID)
use_data(sdtm_example_dataverse_dm)

#EG Domain
sdtm_example_dataverse_eg <-
  read.csv(file="Harvard directory/eg.csv", stringsAsFactors=FALSE)
use_data(sdtm_example_dataverse_eg)

#LB Domain
sdtm_example_dataverse_lb <-
  read.csv(file="Harvard directory/lb.csv", stringsAsFactors=FALSE)
sdtm_example_dataverse_lb$LBORNRLO <- as.character(sdtm_example_dataverse_lb$LBORNRLO)
sdtm_example_dataverse_lb$LBORNRHI <- as.character(sdtm_example_dataverse_lb$LBORNRHI)
use_data(sdtm_example_dataverse_lb)

#MH Domain
sdtm_example_dataverse_mh <-
  read.csv(file="Harvard directory/mh.csv", stringsAsFactors=FALSE)
use_data(sdtm_example_dataverse_mh)

#PE Domain
sdtm_example_dataverse_pe <-
  read.csv(file="Harvard directory/pe.csv", stringsAsFactors=FALSE)
use_data(sdtm_example_dataverse_pe)

#VS Domain
sdtm_example_dataverse_vs <-
  read.csv(file="Harvard directory/vs.csv", stringsAsFactors=FALSE)
use_data(sdtm_example_dataverse_vs)

#EX Domain
sdtm_example_dataverse_ex <-
  read.csv(file="Harvard directory/ex.csv", stringsAsFactors=FALSE)
use_data(sdtm_example_dataverse_ex)

#PC Domain
sdtm_example_dataverse_pc <-
  read.csv(file="Harvard directory/pc.csv", stringsAsFactors=FALSE)
use_data(sdtm_example_dataverse_pc)

