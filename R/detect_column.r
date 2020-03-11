# setwd('C:/Users/dengj25/Pfizer/AMP/Rsdtm/Rsdtm - test/R')
# # dat <- read.csv("B7801001.brussels.demog.csv",as.is = T)
# 
library(rio)
library(tidyverse)
# library(readxl)
library(DescTools)
library(xts)

# 
# domain_spec_filnames <-
#   list.files(pattern=".csv$")
# names(domain_spec_filnames) <- substr(domain_spec_filnames, 1, 2)
# domain_specs <-
#   lapply(
#     X=domain_spec_filnames,
#     FUN=import
#   )
# 
# # Read in controlled terminology dataset
# terminology <- read_excel('SDTM Terminology.xls', sheet='SDTM Terminology 2019-12-20')
# # Read in ISO 3166-1 Alpha-3 Country codes
# data("d.countries")
# CO <- domain_specs$CO
# DM <- domain_specs$DM
# SE <- domain_specs$SE
# SM <- domain_specs$SM
# SV <- domain_specs$SV
# 
# setwd('C:/Users/dengj25/Pfizer/AMP/Rsdtm/Branch2/Rsdtm/R')
# 
# # Adding datasets to package
# usethis::use_data(CO)
# usethis::use_data(DM)
# usethis::use_data(SE)
# usethis::use_data(SM)
# usethis::use_data(SV)
# usethis::use_data(terminology)


#' Check if required columns exist, give an error if they do not exist.
#' 
#' @param data The dataset whose columns to check.
#' @param domain_spec The domain specification ("CO", "DM", "SE", "SM", "SV") for checking columns.
#' @return If required columns are missing, give an error and indicate which required columns are missing.
#' @export
validate_required_column_names <- function(data, domain_spec) {
  if(domain_spec=="CO"){
    data("CO")
    domain_spec <- force(CO)
  } else if (domain_spec=="DM"){
    data("DM")
    domain_spec <- force(DM)
  } else if (domain_spec=="SE"){
    data("SE")
    domain_spec <- force(SE)
  } else if (domain_spec=="SM"){
    data("SM")
    domain_spec <- force(SM)
  } else if (domain_spec=="SV"){
    data("SV")
    domain_spec <- force(SV)
  }
  
  required_cols <- domain_spec$`Variable Name`[domain_spec$Core %in% "Req"]
  missing_requred <- setdiff(required_cols, names(data))
  if (length(missing_requred) > 0) {
    stop(
      "The following required columns are missing: ",
      paste(missing_requred, collapse=", ")
    )
  }
}

# validate_required_column_names(dat, domain_spec=domain_specs$DM)

#' Check if expected columns exist, give a warning if they do not exist
#' 
#' @param data The dataset whose columns to check.
#' @param domain_spec The domain specification ("CO", "DM", "SE", "SM", "SV") for checking columns.
#' @return If expected columns are missing, give an warning and indicate which expected columns are missing.
#' @export
validate_expected_column_names <- function(data, domain_spec) {
  if(domain_spec=="CO"){
    data("CO")
    domain_spec <- force(CO)
  } else if (domain_spec=="DM"){
    data("DM")
    domain_spec <- force(DM)
  } else if (domain_spec=="SE"){
    data("SE")
    domain_spec <- force(SE)
  } else if (domain_spec=="SM"){
    data("SM")
    domain_spec <- force(SM)
  } else if (domain_spec=="SV"){
    data("SV")
    domain_spec <- force(SV)
  }
  expected_cols <- domain_spec$`Variable Name`[domain_spec$Core %in% "Exp"]
  missing_expected <- setdiff(expected_cols, names(data))
  if (length(missing_expected) > 0) {
    warning(
      "The following expected columns are missing: ",
      paste(missing_expected, collapse=", ")
    )
  } 
}

# validate_expected_column_names(dat, domain_spec=domain_specs$DM)

#' Check if permitted columns exist, no message either way
#' 
#' @param data The dataset whose columns to check.
#' @param domain_spec The domain specification ("CO", "DM", "SE", "SM", "SV") for checking columns.
#' @return No message regardless of whether permitted columns are missing.
#' @export
validate_permitted_column_names <- function(data, domain_spec) {
  if(domain_spec=="CO"){
    data("CO")
    domain_spec <- force(CO)
  } else if (domain_spec=="DM"){
    data("DM")
    domain_spec <- force(DM)
  } else if (domain_spec=="SE"){
    data("SE")
    domain_spec <- force(SE)
  } else if (domain_spec=="SM"){
    data("SM")
    domain_spec <- force(SM)
  } else if (domain_spec=="SV"){
    data("SV")
    domain_spec <- force(SV)
  }
  permitted_cols <- domain_spec$`Variable Name`[domain_spec$Core %in% "Perm"]
  missing_permitted <- setdiff(permitted_cols, names(data))
  if (length(missing_permitted) > 0) {
    return(NULL)
  } 
}

# validate_permitted_column_names(dat, domain_spec=domain_specs$DM)

#' Check if extra columns exist, give an warning if they do exist
#' 
#' @param data The dataset whose columns to check.
#' @param domain_spec The domain specification ("CO", "DM", "SE", "SM", "SV") for checking columns.
#' @return If extra columns exist, give an warning and indicate which extra columns exist.
#' @export
validate_extra_column_names <- function(data, domain_spec) {
  if(domain_spec=="CO"){
    data("CO")
    domain_spec <- force(CO)
  } else if (domain_spec=="DM"){
    data("DM")
    domain_spec <- force(DM)
  } else if (domain_spec=="SE"){
    data("SE")
    domain_spec <- force(SE)
  } else if (domain_spec=="SM"){
    data("SM")
    domain_spec <- force(SM)
  } else if (domain_spec=="SV"){
    data("SV")
    domain_spec <- force(SV)
  }
  all_cols <- domain_spec$`Variable Name`
  extra_cols <- setdiff(names(data),all_cols)
  if (length(extra_cols) > 0) {
    warning(
      "The following extra columns do exist: ",
      paste(extra_cols, collapse=", ")
    )
  } 
}

# validate_extra_column_names(dat, domain_spec=domain_specs$DM)

#' Check to see if columns have the expected class (numeric or character), if not give a warning and convert to one or the other. Also, check to see if columns contain NA, give an error if they do.
#' 
#' @param data The dataset whose columns to check.
#' @param domain_spec The domain specification ("CO", "DM", "SE", "SM", "SV") for checking columns.
#' @return If columns are numeric but should be character, give a warning and indicate which are those columns. If columns are character but should be numeric, give a warning and indicate those columns. If columns have NA, give a warning and indicate which are those columns. 
#' @export
validate_column_class <- function(data,domain_spec){
  if(domain_spec=="CO"){
    data("CO")
    domain_spec <- force(CO)
  } else if (domain_spec=="DM"){
    data("DM")
    domain_spec <- force(DM)
  } else if (domain_spec=="SE"){
    data("SE")
    domain_spec <- force(SE)
  } else if (domain_spec=="SM"){
    data("SM")
    domain_spec <- force(SM)
  } else if (domain_spec=="SV"){
    data("SV")
    domain_spec <- force(SV)
  }
  dat1 <- lapply(data,class)
  dat1 <- data.frame(Variable = names(unlist(dat1)), Type = unlist(dat1))
  dat1$Variable <- as.character(dat1$Variable)
  dat1$Type <- as.character(dat1$Type)
  names(domain_spec)[names(domain_spec)=='Variable Name'] <- 'Variable'
  domain_spec_type <- select(domain_spec,Variable,Type)
  # dat1$Type[dat1$Type %in% c('integer','numeric','logical')] <- 'Num'
  dat1$Type[dat1$Type == 'integer'] <- 'Num'
  dat1$Type[dat1$Type == 'numeric'] <- 'Num'
  dat1$Type[dat1$Type == 'logical'] <- 'Num'
  dat1$Type[dat1$Type == 'character'] <- 'Char'
  dat1 <- left_join(dat1,domain_spec_type,by='Variable')
  names_num_char <- dat1$Variable[dat1$Type.x == 'Num' & dat1$Type.y== 'Char']
  names_num_char <- names_num_char[is.na(names_num_char)==F]
  
  if(length(names_num_char)>0){
  warning(
    "The following columns of class 'numeric' or 'logical' should be and have been converted to character: ",
    paste(names_num_char[is.na(names_num_char)==F], collapse=", ")
  )
  }
  
  names_char_num <- dat1$Variable[dat1$Type.x == 'Char' & dat1$Type.y== 'Num']
  names_char_num <- names_char_num[is.na(names_char_num)==F]
  
  if(length(names_char_num)>0) {
  warning(
    "The following columns of class 'character' should be and have been converted to numeric: ",
    paste(names_char_num[is.na(names_char_num)==F], collapse=", ")
  )
  }
  
  data_sub <- data
  
  if(length(names_num_char)>0){
  for (i in names_num_char){
    data_sub[[i]] <- as.character(data[[i]])
  }
  }
  
  if(length(names_char_num)>0) {
  for (j in names_char_num){
    data_sub[[j]] <- as.numeric(data[[j]])
  }
  }
  # Modify input to data at global level
  # data_name <- deparse(substitute(data))
  # assign(data_name,data_sub,envir=parent.frame())
  
  na_names <- list()
  for(i in names(data)){
    if(any(is.na(data[[i]]))){
      na_names[[i]] <- i
    }
  }
  na_names <- unlist(na_names)
  if(length(na_names) > 0) {
  stop(
    "The following columns in dataset have NA: ",
    paste(na_names, collapse=", ")
  )
  }
  data <- data_sub
  data
}

# validate_column_class(dat,domain_spec=domain_specs$DM)

#' Check to see if columns have controlled terminology, give an error if columns do not have controlled terminology.
#' 
#' @param data The dataset whose columns to check.
#' @param domain_spec The domain specification ("CO", "DM", "SE", "SM", "SV") for checking columns.
#' @return If columns contain do not have controlled terminology, give an error and indicate which are those columns.
#' @export
validate_ctrl_terminology <- function(data,domain_spec){
  if(domain_spec=="CO"){
    data("CO")
    domain_spec <- force(CO)
  } else if (domain_spec=="DM"){
    data("DM")
    domain_spec <- force(DM)
  } else if (domain_spec=="SE"){
    data("SE")
    domain_spec <- force(SE)
  } else if (domain_spec=="SM"){
    data("SM")
    domain_spec <- force(SM)
  } else if (domain_spec=="SV"){
    data("SV")
    domain_spec <- force(SV)
  }
  data("terminology")
  terminology <- force(terminology)
  term <- unlist(regmatches(domain_spec$'Controlled Terms, Codelist or Format', 
                            gregexpr("(?=\\().*?(?<=\\))", 
                                     domain_spec$'Controlled Terms, Codelist or Format', perl=T)))
  term2 <- gsub("[()]","",term)
  dat_term <- terminology[terminology$'CDISC Submission Value' %in% term2,]
  dat_term2 <- terminology[terminology$'Codelist Code' %in% dat_term$Code,]
  dat_term$`Codelist Code` <- NULL
  names(dat_term)[names(dat_term) =='Code'] <- 'Codelist Code'
  dat_term2 <- left_join(dat_term2,select(dat_term,'Codelist Code','CDISC Submission Value'),
                         by='Codelist Code')
  ctrl_variable <- domain_spec[domain_spec$'Controlled Terms, Codelist or Format' %in% term,]
  ctrl_variable$'Controlled Terms, Codelist or Format' <- gsub("[()]","",
                                                                 ctrl_variable$'Controlled Terms, Codelist or Format')
  names(dat_term2)[names(dat_term2)=='CDISC Submission Value.y'] <- 'Controlled Terms, Codelist or Format'
  dat_term2 <- left_join(dat_term2,select(ctrl_variable,'Controlled Terms, Codelist or Format',
                                          'Variable Name'),by='Controlled Terms, Codelist or Format')
  ctrl_variable <- ctrl_variable[ctrl_variable$'Variable Name' %in% names(data),]
  noncontrol_col <- list()
  for(i in ctrl_variable$'Variable Name'){
    submission_value <- dat_term2[dat_term2$'Variable Name' %in% i,]$'CDISC Submission Value.x'
    if((any(data[[i]] %in% submission_value) == F)){
      noncontrol_col[[i]] <- i
    }
  }
  noncontrol_col <- unlist(noncontrol_col)
  if(length(noncontrol_col)>0){
  stop(
    "The following columns has entries not consistent with controlled terminology: ",
    paste(noncontrol_col, collapse=", ")
  )
  }
}

# validate_ctrl_terminology(dat,domain_spec = domain_specs$DM)


#' If the dataset has COUNTRY column, check to see if the COUNTRY code is consistent with ISO 3166-1 Alpha-3.
#' 
#' @param data The dataset whose columns to check.
#' @param domain_spec The domain specification ("CO", "DM", "SE", "SM", "SV") for checking columns.
#' @return Give an error if the COUNTRY code is not consistent with ISO 3166-1 Alpha-3.
#' @export
validate_iso <- function(data,domain_spec){
  if(domain_spec=="CO"){
    data("CO")
    domain_spec <- force(CO)
  } else if (domain_spec=="DM"){
    data("DM")
    domain_spec <- force(DM)
  } else if (domain_spec=="SE"){
    data("SE")
    domain_spec <- force(SE)
  } else if (domain_spec=="SM"){
    data("SM")
    domain_spec <- force(SM)
  } else if (domain_spec=="SV"){
    data("SV")
    domain_spec <- force(SV)
  }
  data("d.countries")
  d.countries <- force(d.countries)
  dat_iso <- domain_spec[grep('ISO',domain_spec$'Controlled Terms, Codelist or Format'),]
  noniso_date_col <- list()
  if('ISO 3166-1 Alpha-3' %in% dat_iso$'Controlled Terms, Codelist or Format'){
    country <- dat_iso[dat_iso$'Controlled Terms, Codelist or Format'=='ISO 3166-1 Alpha-3',]
    country_var <- country$'Variable Name'
    if(any((data[[country_var]] %in% d.countries$a3)==F)){
      stop(
        "The following columns are not consistent with ISO 3166-1 Alpha-3 standards: ",
        paste(country_var,collapse=", ")
      )
    }
  }
  if('ISO 8601' %in% dat_iso$'Controlled Terms, Codelist or Format'){
    date <- dat_iso[dat_iso$'Controlled Terms, Codelist or Format'=='ISO 8601',]
    for(i in date$'Variable Name'){
      for(j in data[[i]]){
        date_list <- NULL
        tmp <- as.data.frame(suppressWarnings(.parseISO8601(j)))
        date_list <- rbind(date_list,tmp)
        if(any(is.na(date_list$first.time))){
          noniso_date_col[[i]] <- i
        }
      }
    }
  }
  noniso_date_col <- unlist(noniso_date_col)
  if(length(noniso_date_col > 0)){
    stop(
      "The following columns are not consisent with ISO 8601: ",
      paste(noniso_date_col, collapse=", ")
    )
  }
}

# validate_iso(dat,domain_spec = domain_specs$DM)
