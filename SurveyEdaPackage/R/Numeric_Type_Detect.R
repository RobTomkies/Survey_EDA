#' Numeric Data Type Detection and Forcing
#'
#' This function receives a dataframe and outputs the dataframe with numeric columns
#' either forced to integer or floating point or detected and converted to one or the other. The user can
#' force columns to a certain data type via specifying manually or allow a
#' number of tests on data characteristics to automatically detect if numeric or not.
#' In the common case for survey data where users
#' can select categories or enter free text and the data is included in one
#' column, the free text can be split out and preserved in additional columns
#' that will be added to the dataset.
#'
#' It is recommended that if not forcing but auto detection that function data_type_detect() is used instead.
#'
#'
#' @details
#' Categorical data detection where unforced is split in to two stages.
#'
#' - Firstly whether the data is numeric or not by seeing if over 60% is numeric information.
#' - If this is so and the data is less than 20 records long, if over 90% of the data is integer based then overall we classify as integer
#' - If numeric and over 20 records long, if over 95% is integer then we classify as interger
#' - If either of the last two steps fail but still over 60% numeric information we classify as floating point numeric data

#'
#' Of course if this is ineffective at detecting columns can be manually forced.
#'
#' @param input_vector vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced to nominal data type
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#' @param preserve_nonconform boolean ; value (T of F) on whether to separate and store data that does not conform to the detected/specified column type in an additional column
#' @param force boolean ; (T of F) of whether you wish to force the specified columns or look to automatically detect from within them
#'
#' @return dataframe containing the adjusted dataset
#'
#' @export
#' @examples
#' Numeric_Type_Detect(c('col1','col2'), basic_test_data, preserve_nonconform = T, force = F)
#'
Numeric_Type_Detect <- function(input_vector, dataset, preserve_nonconform = T, force = F){
  #clean up input_vector
  input_vector <- column_recog_vector('numeric', input_vector, dataset)

  #initiate dataframe to hold any split out values if needed
  split_column_hold <-  data.frame(matrix(ncol = length(input_vector), nrow = nrow(dataset)))

  integer_record <- rep(F, length(input_vector))
  float_record <- rep(F, length(input_vector))

  #action loop for each data column
  for(i in 1:length(input_vector)){
    #prop of non NA values that are non numeric
    Numprop <- (sum(is.na(suppressWarnings(as.numeric(dataset[,input_vector[i]])))) - (length(dataset[,input_vector[i]])- length(dataset[,input_vector[i]][!is.na(dataset[,input_vector[i]])])))/length(dataset[,input_vector[i]])

    #greater than 60% numeric information - numeric
    if(Numprop <= 0.4 | force == T){
      if(Numprop == 0){
        #all numeric
        intnum_Data <- dataset[,input_vector[i]][!is.na(dataset[,input_vector[i]])]
        #if 10 data points and 9 are int set to int
        if(sum(intnum_Data==suppressWarnings(as.integer(intnum_Data)), na.rm = T)/length(intnum_Data) >= 0.9 & length(intnum_Data) <= 10){
          dataset[,input_vector[i]] <- suppressWarnings(as.integer(dataset[,input_vector[i]]))
          integer_record[i] <- T
        }
        #otherwise if longer than 10 and 95%
        else if(sum(intnum_Data==suppressWarnings(as.integer(intnum_Data)), na.rm = T)/length(intnum_Data) >= 0.95 & length(intnum_Data) > 10){
          dataset[,input_vector[i]] <- suppressWarnings(as.integer(dataset[,input_vector[i]]))
          integer_record[i] <- T
        }
        #otherwise set to double/float
        else{
          dataset[,input_vector[i]] <- suppressWarnings(as.numeric(dataset[,input_vector[i]]))
          float_record[i] <- T
        }
      }
      else{
        if(force != T){
          warning(paste('60-99.9 percent numeric information present in column', names(dataset)[input_vector[i]],' so treated as such'))
        }
        else if(force == T){
          warning(paste('Not all data was numeric in column', names(dataset)[input_vector[i]], 'but forced'))
        }

        name_trigger <- F
        if(preserve_nonconform == T){
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

          #split out the unknow factors which could be free text
          alternate_data <- dataset[,input_vector[i]]
          #set the ones that are numeric to NA
          alternate_data[!is.na(suppressWarnings(as.numeric(alternate_data)))] <- NA

          #convert the ones we can to numeric
          dataset[,input_vector[i]] <- suppressWarnings(as.numeric(dataset[,input_vector[i]]))

          #int detection - split out NAs
          intnum_Data <- dataset[,input_vector[i]][!is.na(dataset[,input_vector[i]])]
          #if 10 data points and 9 are int set to int
          if(sum(intnum_Data==suppressWarnings(as.integer(intnum_Data)), na.rm = T)/length(intnum_Data) >= 0.9 & length(intnum_Data) <= 10){
            dataset[,input_vector[i]] <- suppressWarnings(as.integer(dataset[,input_vector[i]]))
            integer_record[i] <- T
          }
          #otherwise if longer than 10 and 95%
          else if(sum(intnum_Data==suppressWarnings(as.integer(intnum_Data)), na.rm = T)/length(intnum_Data) >= 0.95 & length(intnum_Data) > 10){
            dataset[,input_vector[i]] <- suppressWarnings(as.integer(dataset[,input_vector[i]]))
            integer_record[i] <- T
          }
          #otherwise set to double/float
          else{
            dataset[,input_vector[i]] <- suppressWarnings(as.numeric(dataset[,input_vector[i]]))
            float_record[i] <- T
          }


          #shift unknown to storage column
          split_column_hold[i] <- alternate_data
          rm(alternate_data)
        }
        #if not wanting to preserve other values
        else{
          intnum_Data <- dataset[,input_vector[i]][!is.na(dataset[,input_vector[i]])]
          #if 10 data points and 9 are int set to int
          if(sum(intnum_Data==suppressWarnings(as.integer(intnum_Data)), na.rm = T)/length(intnum_Data) >= 0.9 & length(intnum_Data) <= 10){
            dataset[,input_vector[i]] <- suppressWarnings(as.integer(dataset[,input_vector[i]]))
            integer_record[i] <- T
          }
          #otherwise if longer than 10 and 95%
          else if(sum(intnum_Data==suppressWarnings(as.integer(intnum_Data)), na.rm = T)/length(intnum_Data) >= 0.95 & length(intnum_Data) > 10){
            dataset[,input_vector[i]] <- suppressWarnings(as.integer(dataset[,input_vector[i]]))
            integer_record[i] <- T
          }
          #otherwise set to double/float
          else{
            dataset[,input_vector[i]] <- suppressWarnings(as.numeric(dataset[,input_vector[i]]))
            float_record[i] <- T
          }
        }
      }
    }
    #less than 60% numeric information
    #continue
  }
  split_column_hold <- Filter(function(x)!all(is.na(x)), split_column_hold)
  dataset <- cbind(dataset, split_column_hold)
  if(force == T){
    return(list(integers = input_vector[integer_record], floats = input_vector[float_record], data = dataset))
  }
  else if(force == F){
    return(list(integers = input_vector[integer_record], floats = input_vector[float_record],data = dataset))
  }
}


