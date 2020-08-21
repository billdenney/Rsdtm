#' Example SDTM Adverse Events Data
#'
#' @format A data frame with 18 variables and 16 observations:
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
#'    \item{AEENRF}{End Relative to Reference Period, character}
#' }
#' @family SDTM example data
#' @source \url{https://doi.org/10.7910/DVN/51B6NK}
"sdtm_example_dataverse_ae"


#' Example SDTM Demographics Data
#'
#' @format A data frame with 16 variables and 5 observations:
#' \describe{
#'  \item{STUDYID }{Study Identifier, character}
#'  \item{DOMAIN }{Domain Abbreviation, character}
#'  \item{USUBJID }{Unique Subject Identifier, character}
#'  \item{SUBJID }{Subject Identifier for the Study, character}
#'  \item{RFSTDTC }{Subject Reference Start Date/Time, character, ISO 8601}
#'  \item{RFENDTC }{Subject Reference End Date/Time, character, ISO 8601}
#'  \item{SITEID }{Study Site Identifier, character}
#'  \item{BRTHDTC }{Date/Time of Birth, character, ISO 8601}
#'  \item{AGE }{Age, numeric}
#'  \item{AGEU }{Age Units, character}
#'  \item{SEX }{Sex, character}
#'  \item{RACE }{Race , character}
#'  \item{ETHNIC }{Ethnicity, character}
#'  \item{ARMCD }{Planned Arm Code, character}
#'  \item{ARM }{Description of Planned Arm, character}
#'  \item{COUNTRY }{Country, character}
#' }
#' @family SDTM example data
#' @source \url{https://doi.org/10.7910/DVN/51B6NK}
"sdtm_example_dataverse_dm"


#' Example SDTM ECG Test Results Data
#'
#' @format A data frame with 21 variables and 56 observations:
#' \describe{
#'  \item{STUDYID}{Study Identifier, character}
#'  \item{DOMAIN}{Domain Abbreviation, character}
#'  \item{USUBJID}{Unique Subject Identifier, character}
#'  \item{EGSEQ}{Sequence Number, numeric}
#'  \item{EGSPID}{Sponsor-Defined Identifier, character}
#'  \item{EGTESTCD}{ECG Test or Examination Short Name, character}
#'  \item{EGTEST}{ECG Test or Examination Name, character}
#'  \item{EGPOS}{ECG Position of Subject, character}
#'  \item{EGORRES}{Result or Finding in Original Units, character}
#'  \item{EGORRESU}{Original Units, character}
#'  \item{EGSTRESC}{Character Result/Finding in Std Format, character}
#'  \item{EGSTRESN}{Numeric Result/Finding in Standard Units, numeric}
#'  \item{EGSTRESU}{Standard Units, character}
#'  \item{EGBLFL}{Baseline Flag, character}
#'  \item{EGDRVFL}{Derived Flag, character}
#'  \item{EGEVAL}{Evaluator, character}
#'  \item{VISITNUM}{Visit Number, numeric}
#'  \item{VISIT}{Visit Name, character}
#'  \item{VISITDY}{Planned Study Day of Visit, numeric}
#'  \item{EGDTC}{Date/Time of ECG, character, ISO 8601}
#'  \item{EGDY}{Study Day of ECG, numeric}
#' }
#' @family SDTM example data
#' @source \url{https://doi.org/10.7910/DVN/51B6NK}
"sdtm_example_dataverse_eg"

#' Example SDTM Laboratory Test Results Data
#'
#' @format A data frame with 29 variables and 83 observations:
#' \describe{
#'  \item{STUDYID}{Study Identifier, character}
#'  \item{DOMAIN}{Domain Abbreviation, character}
#'  \item{USUBJID}{Unique Subject Identifier, character}
#'  \item{LBSEQ}{Sequence Number, numeric}
#'  \item{LBREFID}{Specimen ID, character}
#'  \item{LBTESTCD}{Lab Test or Examination Short Name, character}
#'  \item{LBTEST}{Lab Test or Examination Name, character}
#'  \item{LBCAT}{Category for Lab Test, character}
#'  \item{LBSCAT}{Subcategory for Lab Test, character}
#'  \item{LBORRES}{Result or Finding in Original Units, character}
#'  \item{LBORRESU}{Original Units, character}
#'  \item{LBORNRLO}{Reference Range Lower Limit in Orig Unit, character}
#'  \item{LBORNRHI}{Reference Range Upper Limit in Orig Unit, character}
#'  \item{LBSTRESC}{Character Result/Finding in Std Format, character}
#'  \item{LBSTRESN}{Numeric Result/Finding in Standard Units, numeric}
#'  \item{LBSTRESU}{Standard Units, character}
#'  \item{LBSTNRLO}{Reference Range Lower Limit-Std Units, numeric}
#'  \item{LBSTNRHI}{Reference Range Upper Limit-Std Units, numeric}
#'  \item{LBSTNRC}{Reference Range for Char Rslt-Std Units, character}
#'  \item{LBNRIND}{Reference Range Indicator, character}
#'  \item{LBSPEC}{Specimen Type, character}
#'  \item{LBMETHOD}{Method of Test or Examination, character}
#'  \item{LBBLFL}{Baseline Flag, character}
#'  \item{LBFAST}{Fasting Status, character}
#'  \item{VISITNUM}{Visit Number, numeric}
#'  \item{VISIT}{Visit Name, character}
#'  \item{VISITDY}{Planned Study Day of Visit, numeric}
#'  \item{LBDTC}{Date/time of Specimen Collection, character, ISO 8601}
#'  \item{LBDY}{Study Day of Specimen Collection, numeric}
#' }
#' @family SDTM example data
#' @source \url{https://doi.org/10.7910/DVN/51B6NK}
"sdtm_example_dataverse_lb"


#' Example SDTM Medical History Data
#'
#' @format A data frame with 12 variables and 18 observations:
#' \describe{
#'  \item{STUDYID}{Study Identifier, character}
#'  \item{DOMAIN}{Domain Abbreviation, character}
#'  \item{USUBJID}{Unique Subject Identifier, character}
#'  \item{MHSEQ}{Sequence Number, numeric}
#'  \item{MHTERM}{Reported Term for the Medical History, character}
#'  \item{MHCAT}{Category for Medical History, character}
#'  \item{MHPRESP}{Medical History Event Pre- Specified, character}
#'  \item{MHOCCUR}{Medical History Occurrence, character}
#'  \item{MHBODSYS}{Body System or Organ Class, character}
#'  \item{MHDTC}{Date/Time of History Collection, character, ISO 8601}
#'  \item{MHSTDTC}{Start Date/Time of Medical History Event, character, ISO 8601}
#'  \item{MHENRF}{End Relative to Reference Period, character}
#'  }
#' @family SDTM example data
#' @source \url{https://doi.org/10.7910/DVN/51B6NK}
"sdtm_example_dataverse_mh"


#' Example SDTM Physical Examination Data
#'
#' @format A data frame with 13 variables and 65 observations:
#' \describe{
#'  \item{STUDYID}{Study Identifier, character}
#'  \item{DOMAIN}{Domain Abbreviation, character}
#'  \item{USUBJID}{Unique Subject Identifier, character}
#'  \item{PESEQ}{Sequence Number, numeric}
#'  \item{PETESTCD}{Body System Examined Short Name, character}
#'  \item{PETEST}{Body System Examined, character}
#'  \item{PEORRES}{Verbatim Examination Finding, character}
#'  \item{PESTRESC}{Character Result/Finding in Standard Format, character}
#'  \item{VISITNUM}{Visit Number, numeric}
#'  \item{VISIT}{Visit Name, character}
#'  \item{VISITDY}{Planned Study Day of Visit, numeric}
#'  \item{PEDTC}{Date/Time of Examination, character, ISO 8601}
#'  \item{PEDY}{Study Day of Examination, numeric}
#'  }
#' @family SDTM example data
#' @source \url{https://doi.org/10.7910/DVN/51B6NK}
"sdtm_example_dataverse_pe"


#' Example SDTM Vital Signs Data
#'
#' @format A data frame with 18 variables and 72 observations:
#' \describe{
#'  \item{STUDYID}{Study Identifier, character}
#'  \item{DOMAIN}{Domain Abbreviation, character}
#'  \item{USUBJID}{Unique Subject Identifier, character}
#'  \item{VSSEQ}{Sequence Number, numeric}
#'  \item{VSTESTCD}{Vital Signs Test Short Name, character}
#'  \item{VSTEST}{Vital Signs Test Name, character}
#'  \item{VSPOS}{Vital Signs Position of Subject, character}
#'  \item{VSORRES}{Result or Finding in Original Units, character}
#'  \item{VSORRESU}{Original Units, character}
#'  \item{VSSTRESC}{Character Result/Finding in Std Format, character}
#'  \item{VSSTRESN}{Numeric Result/Finding in Standard Units, numeric}
#'  \item{VSSTRESU}{Standard Units, character}
#'  \item{VSBLFL}{Baseline Flag, character}
#'  \item{VISITNUM}{Visit Number, numeric}
#'  \item{VISIT}{Visit Name, character}
#'  \item{VISITDY}{Planned Study Day of Visit, numeric}
#'  \item{VSDTC}{Date/Time of Measurements, character, ISO 8601}
#'  \item{VSDY}{Study Day of Vital Signs, numeric}
#'  }
#' @family SDTM example data
#' @source \url{https://doi.org/10.7910/DVN/51B6NK}
"sdtm_example_dataverse_vs"


#' Example SDTM Exposure Data
#'
#' @format A data frame with 9 variables and 12 observations:
#' \describe{
#'  \item{STUDYID}{Study Identifier, character}
#'  \item{DOMAIN}{Domain Abbreviation, character}
#'  \item{SUBJID}{Subject Identifier for the Study, character}
#'  \item{USUBJID}{Unique Subject Identifier, character}
#'  \item{EXSEQ}{Sequence Number, numeric}
#'  \item{EXTRT}{Name of Treatment, character}
#'  \item{EXDOSE}{Dose, Numeric}
#'  \item{EXDOSU}{Dose Units, character}
#'  \item{EXROUTE}{Route of Administration, character}
#'  \item{EXSTDTC}{Start Date/Time of Treatment, character, ISO 8601}
#' }
#' @family SDTM example data
"sdtm_example_dataverse_ex"


#' Example SDTM Pharmacokinetics Concentration Data
#'
#' @format A data frame with 15 variables and 132 observations:
#' \describe{
#'  \item{STUDYID}{Study Identifier, character}
#'  \item{DOMAIN}{Domain Abbreviation, character}
#'  \item{SUBJID}{Subject Identifier for the Study, character}
#'  \item{USUBJID}{Unique Subject Identifier, character}
#'  \item{PCSEQ}{Sequence Number, numeric}
#'  \item{PCTESTCD}{Pharmacokinetic Test Short Name, character}
#'  \item{PCTEST}{Pharmacokinetic Test Name, character}
#'  \item{PCORRES}{Result or Finding in Original Units, character}
#'  \item{PCORRESU}{Original Units, character}
#'  \item{PCSTRESC}{Character Result/Finding in Standard Format, character}
#'  \item{PCSTRESN}{Numeric Result/Finding in Standard Units, numeric}
#'  \item{PCSTRESU}{Standard Units, character}
#'  \item{PCDTC}{Date/Time of Specimen Collection, character, ISO 8601}
#'  \item{PCTPT}{Planned Time Point Name, character}
#'  \item{PCTPTNUM}{Planned Time Point Number, numeric}
#'  }
#' @family SDTM example data
"sdtm_example_dataverse_pc"

