#' Check if dataset is consistent with SDTMIG specifications.
#'
#' @param data The dataset whose columns to check.
#' @param sdtmig_version The version of SDTMIG (e.g. "SDTMIG3.3").
#' @param domain_name The domain specification ("CO", "DM", "SE", "SM", "SV") for checking columns.
#' @return If dataset is not consistent with SDTMIG specifications, give a warning or error and convert class-misspecified columns to specified class.
#' @export
validate_column_names <- function(data, sdtmig_version, domain_name) {
  
  # Check if required columns exist, give an error if they do not exist.
  validate_required_column_names <- function(data, sdtmig_version, domain_name) {
    domain_spec <- domain_spec_list[[sdtmig_version]][[domain_name]]
    required_cols <- domain_spec$`Variable Name`[domain_spec$Core %in% "Req"]
    missing_requred <- setdiff(required_cols, names(data))
    if (length(missing_requred) > 0) {
      stop(
        "The following required columns are missing: ",
        paste(missing_requred, collapse=", ")
      )
    }
  }
  
    validate_required_column_names(data, sdtmig_version, domain_name)
    
   # Check if expected columns exist, give a warning if they do not exist
    validate_expected_column_names <- function(data,sdtmig_version, domain_name) {
      domain_spec <- domain_spec_list[[sdtmig_version]][[domain_name]]
      expected_cols <- domain_spec$`Variable Name`[domain_spec$Core %in% "Exp"]
      missing_expected <- setdiff(expected_cols, names(data))
      if (length(missing_expected) > 0) {
        warning(
          "The following expected columns are missing: ",
          paste(missing_expected, collapse=", ")
        )
      }
    }
    
    validate_expected_column_names(data, sdtmig_version, domain_name)
    
    # Check if permitted columns exist, no message either way
    validate_permitted_column_names <- function(data,sdtmig_version, domain_name) {
      domain_spec <- domain_spec_list[[sdtmig_version]][[domain_name]]
      permitted_cols <- domain_spec$`Variable Name`[domain_spec$Core %in% "Perm"]
      missing_permitted <- setdiff(permitted_cols, names(data))
      if (length(missing_permitted) > 0) {
        return(NULL)
      }
    }
    
    validate_permitted_column_names(data,sdtmig_version, domain_name)
    
    # Check if extra columns exist, give an warning if they do exist
    validate_extra_column_names <- function(data,sdtmig_version, domain_name) {
      domain_spec <- domain_spec_list[[sdtmig_version]][[domain_name]]
      all_cols <- domain_spec$`Variable Name`
      extra_cols <- setdiff(names(data),all_cols)
      if (length(extra_cols) > 0) {
        warning(
          "The following extra columns do exist: ",
          paste(extra_cols, collapse=", ")
        )
      }
    }
    
    validate_extra_column_names(data,sdtmig_version, domain_name)
    
    # Check to see if columns have controlled terminology, give an error if columns do not have controlled terminology.
    get_cdisc_controlled_terminology <- function(codelist){
      data("terminology")
      terminology <- force(terminology)
      dat_term <- terminology[terminology$'CDISC Submission Value' %in% codelist,]
      dat_term2 <- terminology[terminology$'Codelist Code' %in% dat_term$Code,]
      controlled_terminology <- dat_term2$'CDISC Submission Value'
      return(controlled_terminology)
    }
    
    validate_ctrl_terminology <- function(data,sdtmig_version,domain_name){
      domain_spec <- domain_spec_list[[sdtmig_version]][[domain_name]]
      term <- unlist(regmatches(domain_spec$'Controlled Terms, Codelist or Format',
                                gregexpr("(?=\\().*?(?<=\\))",
                                         domain_spec$'Controlled Terms, Codelist or Format', perl=T)))
      term2 <- gsub("[()]","",term)
      var <- domain_spec[domain_spec$'Controlled Terms, Codelist or Format' %in% term,]$'Variable Name'
      data_name <- names(data[names(data) %in% var])
      
      noncontrol_col <- list()
      
      for(i in 1:length(data_name)){
        terminology <- get_cdisc_controlled_terminology(term2[i])
        if(any(data[[data_name[i]]] %in% terminology) == F){
          noncontrol_col[[i]] <- data_name[i]
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
    
    validate_ctrl_terminology(data,sdtmig_version,domain_name)
    
    # Check to see if columns are consistent with ISO format, give an error if columns are not consistent.
    validate_iso <- function(data,sdtmig_version,domain_name){
      domain_spec <- domain_spec_list[[sdtmig_version]][[domain_name]]
      data("d.countries")
      d_countries <- force(d.countries)
      dat_iso <- domain_spec[grep('ISO',domain_spec$'Controlled Terms, Codelist or Format'),]
      noniso_date_col <- list()
      if('ISO 3166-1 Alpha-3' %in% dat_iso$'Controlled Terms, Codelist or Format'){
        country <- dat_iso[dat_iso$'Controlled Terms, Codelist or Format'=='ISO 3166-1 Alpha-3',]
        country_var <- country$'Variable Name'
        if(any((data[[country_var]] %in% d_countries$a3)==F)){
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
    
    validate_iso(data,sdtmig_version,domain_name)
    
    # Check to see if columns have the expected class (numeric or character), 
    #if not give a warning and convert to one or the other. 
    #Also, check to see if columns contain NA, give an error if they do.
    validate_column_class <- function(data,sdtmig_version,domain_name){
      domain_spec <- domain_spec_list[[sdtmig_version]][[domain_name]]
      dat1 <- lapply(data,class)
      dat1 <- lapply(dat1,function(l) l[[1]])
      dat1 <- data.frame(Variable = names(unlist(dat1)), Type = unlist(dat1))
      dat1$Variable <- as.character(dat1$Variable)
      dat1$Type <- as.character(dat1$Type)
      names(domain_spec)[names(domain_spec)=='Variable Name'] <- 'Variable'
      domain_spec_type <- select(domain_spec,Variable,Type)
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
      
      # na_names <- list()
      # for(i in names(data)){
      #   if(any(is.na(data[[i]]))){
      #     na_names[[i]] <- i
      #   }
      # }
      # na_names <- unlist(na_names)
      # if(length(na_names) > 0) {
      #   stop(
      #     "The following columns in dataset have NA: ",
      #     paste(na_names, collapse=", ")
      #   )
      # }
      
      data <- data_sub
      return(data)
    }
    
    validate_column_class(data,sdtmig_version, domain_name)
}