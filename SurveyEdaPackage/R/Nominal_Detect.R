#' Nominal Data Type Detection and Forcing
#'
#' This function receives a dataframe and outputs the dataframe with nominal columns
#' either forced to nominal or detected and converted to nominal. The user can
#' force columns to a certain data type via specifying manually or allow a
#' number of tests on data characteristics to automatically detect if nominal or not.
#' In the common case for survey data where users
#' can select categories or enter free text and the data is included in one
#' column, the free text can be split out and preserved in additional columns
#' that will be added to the dataset.
#'
#' It is recommended that if not forcing but auto detection that function data_type_detect() is used instead.
#'
#'
#' @details
#' Categorical data detection where unforced is split in to two approaches.
#'
#' Where data is comprised of 20 or fewer records:
#' - a value in the data is considered potentially nominal if over 10% of values are that value
#' - if over 80% of records are made up of potentially categorical values we consider the field as nominal
#'
#' Where data is comprised of 21 or more records:
#' - a value in the data is considered potentially nominal if over 5% of values are that value
#' - if over 90% of records are made up of potentially categorical values we consider the field as nominal
#'
#' Of course if this is ineffective at detecting columns can be manually forced.
#'
#'
#'
#' @param input_vector vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced to nominal data type
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#' @param preserve_nonconform boolean ; value (T of F) on whether to separate and store data that does not conform to the detected/specified column type in an additional column
#' @param force boolean ; (T of F) of whether you wish to force the specified columns or look to automatically detect from within them
#'
#' @return dataframe containing the adjusted dataset
#' @export
#'
#' @examples
Nominal_Detect <- function(input_vector, dataset, preserve_nonconform = T, force = F){
  input_vector <- column_recog_vector('nominal', input_vector, dataset)
  #initiate dataframe to hold any split out values if needed
  split_column_hold <-  data.frame(matrix(ncol = length(input_vector), nrow = nrow(dataset)))
  detect_record <- rep(F, length(input_vector))
  for(i in 1:length(input_vector)){
    working_data <- dataset[,input_vector[i]]
    if(force == T){
      dataset[,input_vector[i]] <- factor(working_data)
    }
    if(length(working_data) <= 20 & force == F){
      categorical <- table(working_data)>(0.1*length(working_data))
      prop_cat <- sum(table(working_data)[categorical])/length(working_data)
      if(prop_cat >= 0.8 & preserve_nonconform == T){
        detect_record[i] <- T
        #set up name trigger to ensure no double naming
        name_trigger <- F
        running_name <- paste(names(dataset)[input_vector[i]], '_other')
        while(name_trigger == F){
          if(running_name %in% names(dataset)){
            running_name <- paste(running_name, '_.')
          }
          else{
            name_trigger <- T
          }
        }
        names(split_column_hold)[i] <- running_name
        alternate_data <- working_data
        #set data thats in categories to NA
        alternate_data[as.character(working_data) %in% (names(table(working_data))[categorical])] <- NA
        #set data thats not in categories to NA
        working_data[!(as.character(working_data) %in% (names(table(working_data))[categorical]))] <- NA
        split_column_hold[i] <- alternate_data
        dataset[,input_vector[i]] <- factor(working_data)
        rm(alternate_data)
        rm(working_data)
      }
      else if(prop_cat >= 0.8 & preserve_nonconform == F){
        detect_record[i] <- T
        working_data[!(working_data %in% names(table(working_data))[categorical])] <- NA
        dataset[,input_vector[i]] <- factor(working_data)
        rm(working_data)
      }
    }
    else if (length(working_data) > 20 & force == F){
      categorical <- table(working_data)>(0.05*length(working_data))
      prop_cat <- sum(table(working_data)[categorical])/length(working_data)
      if(prop_cat >= 0.9 & preserve_nonconform == T){
        detect_record[i] <- T
        #set up name trigger to ensure no double naming
        name_trigger <- F
        running_name <- paste(names(dataset)[input_vector[i]], '_other')
        while(name_trigger == F){
          if(running_name %in% names(dataset)){
            running_name <- paste(running_name, '_.')
          }
          else{
            name_trigger <- T
          }
        }
        names(split_column_hold)[i] <- running_name
        alternate_data <- working_data
        #set data thats in categories to NA
        alternate_data[as.character(working_data) %in% (names(table(working_data))[categorical])] <- NA
        #set data thats not in categories to NA
        working_data[!(as.character(working_data) %in% (names(table(working_data))[categorical]))] <- NA
        split_column_hold[i] <- alternate_data
        dataset[,input_vector[i]] <- factor(working_data)
        rm(alternate_data)
        rm(working_data)
      }
      else if(prop_cat >= 0.95 & preserve_nonconform == F){
        detect_record[i] <- T
        working_data[!(working_data %in% names(table(working_data))[categorical])] <- NA
        dataset[,input_vector[i]] <- factor(working_data)
        rm(working_data)
      }
    }
  }
  split_column_hold <- Filter(function(x)!all(is.na(x)), split_column_hold)
  dataset <- cbind(dataset, split_column_hold)
  if(force == T){
    return(dataset)
  }
  else if(force == F){
    return(list(detected = input_vector[detect_record],data = dataset))
  }
}

