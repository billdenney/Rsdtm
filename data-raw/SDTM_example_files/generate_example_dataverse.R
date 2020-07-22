library(devtools)
library(usethis)

# Generate example SDTM datasets here using the Harvard Dataverse datasets.  The
# name of the dataset should be "sdtm_example_dataverse_" followed by the SDTM
# domain.  The long name prefix (sdtm_example_dataverse_) anticipates that
# additional example may be available in the future.

sdtm_example_dataverse_ae <-
  read.csv(file="Harvard directory/ae.csv", stringsAsFactors=FALSE)
# Confirm that all columns match the expected SDTM type (e.g. character vs
# numeric) here.  To know the class, refer to the SDTM standard for the domain.
sdtm_example_dataverse_ae$AESPID <- as.character(sdtm_example_dataverse_ae$AESPID)
use_data(sdtm_example_dataverse_ae)
