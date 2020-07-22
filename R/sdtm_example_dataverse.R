#' Example SDTM Adverse Events Data
#'
#' @format A data frame with 16 variables and 18 observations:
#' \describe{
#'   \item{STUDYID}{Study Identifier, character}
#'   \item{DOMAIN}{Domain Abbreviation, character}
#'   \item{USUBJID}{Unique Subject Identifier, character}
#'   \item{AESEQ}{Sequence Number, numeric}
#'   \item{AESPID}{Sponsor-Defined Identifier, character}
#'   \item{AETERM}{Reported Term for the Adverse Event, character}
#'   \item{AEMODIFY}{Modified Reported Term, character}
#'   \item{AEDECOD}{Dictionary-Derived Term, character}
#'   \item{AEBODSYS}{Body System or Organ Class, character}
#'   \item{AESEV}{Severity/Intensity, character}
#'   \item{AESER}{Serious Event, character}
#'   \item{AEACN}{Action Taken with Study Treatment, character}
#'   \item{AEREL}{Causality, character}
#'   \item{AESTDTC}{Start Date/Time of Adverse Event, character, ISO8601}
#'   \item{AEENDTC}{End Date/Time of Adverse Event, character, ISO8601}
#'   \item{AESTDY}{Study Day of Start of Adverse Event, numeric}
#'   \item{AEENDY}{Study Day of End of Adverse Event, numeric}
#'   \item{AEENRF}{End Relative to Reference Period, character}
#' }
#' @family SDTM example data
#' @source \url{https://doi.org/10.7910/DVN/51B6NK}
"sdtm_example_dataverse_ae"
