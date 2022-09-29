 trial_dataframe <- data.frame(ordinal_level_uno = c(1,2,3,2,3,1,'hello', 'dragon',1,2,1), x = c(1,2,3,4,3,1,2,2,2,1,4), y = c(2,3,4,6,7,8,9,5,8,4,1), doubls = c(1.1,2,3,4.1,5.2,4.6,7.1,9.7,3.4,4.3,2.1),actual_int = c('1','2','3','4','5','6','7','8', '10','9','99'),words = c('hello', 'my', 'name', 'is', 'rob', 'the', 'great', 'est', 'hero', 'ever', 'like'))
#
# z <- c('x', 'y', 'doubls')
# column_recog_vector('integer', z,  trial_dataframe)



column_recog_vector <- function(force_type, input_vector, dataset){
  output_vector <- rep(NA, length(input_vector))
  if(any(duplicated(input_vector)) & !is.logical(input_vector)){
    warning('type 1: duplicated found in forcing columns - simplified so only one present')
    input_vector <- input_vector[!duplicated(input_vector)]
  }
  #numeric values
  if(is.numeric(input_vector)){
    #check for integer inputs
    if(all(input_vector-floor(input_vector)==0)){
      output_vector <- input_vector
    }
    #check for float input and shift to int
    else if(is.numeric(input_vector)){
      warning(paste('Float type number provided for ', force_type,' forcing so converted to integer\n'))
      output_vector <- as.integer(input_vector)
    }
  }
  #check for logical input (why?)
  else if(is.logical(input_vector)){
    if(length(input_vector) != length(names(dataset))){
      stop(paste('logical forcing vector for ', force_type, ' does not match the length of dataset columns please reconsider \n'))
    }
    output_vector <- which(input_vector == T)
  }
  else if(is.character(input_vector)){
    if(all(input_vector %in% names(dataset))){
      output_vector <- match(input_vector, names(dataset))
    }
    else{
      warning(paste('Not all names provided in ',force_type, ' forcing found in dataset, attempting to coerce\n' ))
    }
    #max length vector could be
    ints_as_strings <- rep(NA,length(input_vector))
    numeric_as_strings <- rep(NA,length(input_vector))
#todo! shift to rcpp
    for(i in 1:length(input_vector)){
      #integer as string and not in names
      if((!(input_vector[i] %in% names(dataset))) & (suppressWarnings(!is.na(as.integer(input_vector[i])))) & (suppressWarnings(as.integer(input_vector[i])) == suppressWarnings(as.numeric(input_vector[i])))){
        ints_as_strings[i] <- input_vector[i]
        output_vector[i] <- as.integer(input_vector[i])
      }
      #numeric but not integer and string
      else if((!(input_vector[i] %in% names(dataset))) & (suppressWarnings(!is.na(as.numeric(input_vector[i])))) & (suppressWarnings(as.integer(input_vector[i])) != suppressWarnings(as.numeric(input_vector[i])))){
        output_vector[i] <- as.integer(input_vector[i])
        numeric_as_strings[i] <- input_vector[i]
      }
      #name of column used
      else if(input_vector[i] %in% names(dataset)){
        output_vector[i] <- which(names(dataset) == input_vector[i])
      }
      else(stop(paste('could not find column name ', input_vector[i], ' from ', force_type, ' forcing. Please reconsider\n')))
    }
    #remove NA values
    ints_as_strings <- ints_as_strings[!is.na(ints_as_strings)]
    numeric_as_strings <- numeric_as_strings[!is.na(numeric_as_strings)]
    if(any(!is.na(ints_as_strings))){
      warning(paste('Integer names ',paste(ints_as_strings, collapse = ', '),' not found for ', force_type,' forcing but successfully coerced to integer index\n', collapse = ""))
    }
    if(any(!is.na(numeric_as_strings))){
      warning(paste('Numeric names ',paste(numeric_as_strings, collapse = ', '),' not found for ', force_type,' forcing but successfully coerced to integer index\n',collapse = ""))
    }
  }
  if(any(duplicated(output_vector))){
    warning('type 2: duplicate columns for forcing found - simplified to only one\n')
    output_vector <- output_vector[!duplicated(output_vector)]
  }
  return(as.integer(output_vector))
}

# x <- list(c('x', 1,2,3), c('doubls', 4,5,6))
# z <- column_recog_list('nominal', x, trial_dataframe)
# z[[2]][[2]]

column_recog_list <- function(force_type, input_list, dataset){
  if(!(force_type %in% c('ordinal', 'alternate_NA'))){stop('invalid force type in column_recog_list')}
  #check list has been supplied
  if(!is.list(input_list)){
    stop(paste('Input for', force_type, 'is not of format list, please see documentation for details of correct input'))
  }
  #check all elements are vectors
  if(!all(sapply(input_list, function(y) is.vector(y) & !is.list(y)))){
    stop(paste('Input components of list for', force_type, 'is not of format vector, please see documentation for details of correct input'))
  }
  #return first element in each vector
  action_cols <- sapply(input_list, function(y){return(y[1])})
  action_levels <- sapply(input_list, function(y){return(y[-1])}, simplify = F)
  if(force_type == 'ordinal'){
    action_cols_index <- column_recog_vector(force_type = 'ordinal', action_cols, dataset)
  }
  else if(force_type == 'alternate_NA'){
    action_cols_index <- column_recog_vector(force_type = 'alternate_NA', action_cols, dataset)
  }
  return(list(c(action_cols_index), action_levels))
}

# trial_data <- trial_dataframe
# x <- list(c('x', 1,2,3), c('doubls', 2,5,6))
# z <- Alternate_NA_Remove(x, trial_data)
#


Alternate_NA_Remove <- function(input_list, dataset){
  if(length(input_list) >= 1){
    adjusted_input_list <- column_recog_list('alternate_NA', input_list, dataset)
    column_indexes <- adjusted_input_list[[1]]
    NA_values <- adjusted_input_list[[2]]
    #!TODO shift to rcpp?
    for(i in 1:length(column_indexes)){
      #check alternate values are found
      if(any(!(NA_values[[i]] %in% dataset[,column_indexes[i]]))){
        warning(paste('Alternate NA values', paste(NA_values[[i]][!(NA_values[[i]] %in% dataset[,column_indexes[i]])], collapse = " "), 'not found in data column', column_indexes[i]))
      }
      #set the alternate NA values to NA
      dataset[,column_indexes[i]][dataset[,column_indexes[i]] %in% NA_values[[i]]] <- NA
    }
  }
  return(dataset)
}


# trial_data <- trial_dataframe
# x <- list(c('ordinal_level_uno', 1,2,3), c('x', 1,2,3)) #, c('doubls', 2,5,6)
# z <- Ordinal_Force(x, trial_data)
# z$ordinal_level_uno

Ordinal_Force <- function(input_list, dataset, preserve_nonconform = T){
  #column name recognition
  adjusted_input_list <- column_recog_list('ordinal', input_list, dataset)
  column_indexes <- adjusted_input_list[[1]]
  ordinal_levels <- adjusted_input_list[[2]]

  #empty dataframe incase new levels need to be created and inputs stored
  split_column_hold <-  data.frame(matrix(ncol = length(column_indexes), nrow = nrow(dataset)))

  #check the levels stated work with whats present
  for(i in 1:length(column_indexes)){
    #number of unique values in nominal column does not match stated values
    if(any(!(ordinal_levels[[i]] %in% unique(dataset[,column_indexes[i]])))){
      stop(paste('Ordinal levels stated are not present in data column', names(dataset)[column_indexes[i]], 'please reconsider'))
    }
    if(length(unique(dataset[,column_indexes[i]])) != length(ordinal_levels[[i]])){
      length_difference <- length(unique(dataset[,column_indexes[i]])) - length(ordinal_levels[[i]])
      if(preserve_nonconform == F){
        warning(paste('Differing number of levels found in data where an additional'), length_difference, 'factors were found, these have been removed')
        #set values that aren't in stated levels and aren't na to auto_unknown
        dataset[,column_indexes[i]][!(dataset[,column_indexes[i]] %in% ordinal_levels[[i]])] <- NA
      }
      else{
        warning(paste('Differing number of levels found in data where an additional'), length_difference, 'factors were found, these have been separated into secondary _other_levels column and factor set to NA')

        #set up name trigger to ensure no double naming
        name_trigger <- F
        running_name <- paste(names(dataset)[column_indexes[i]], '_other_ordinal')
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
        alternate_data <- dataset[,column_indexes[i]]
        #set the ones that are in the levels to NA
        alternate_data[(dataset[,column_indexes[i]] %in% ordinal_levels[[i]])] <- NA

        #set values that aren't in stated levels and aren't na to auto_unknown
        dataset[,column_indexes[i]][!(dataset[,column_indexes[i]] %in% ordinal_levels[[i]])] <- NA

        #shift unknown to storage column
        split_column_hold[i] <- alternate_data
        rm(alternate_data)
      }

    }
    #set column to factor
    dataset[,column_indexes[i]] <- factor(dataset[,column_indexes[i]], levels = ordinal_levels[[i]])
  }
  #remove any columns that were all good with specified levels
  split_column_hold <- Filter(function(x)!all(is.na(x)), split_column_hold)

  #add the split out columns to the dataset
  dataset <- cbind(dataset, split_column_hold)

  return(dataset)
}

#
# trial_data <- trial_dataframe
# x <- c('ordinal_level_uno','x', 'words') #, c('doubls', 2,5,6)
# z <- Numeric_Type_Detect(x, trial_data, F, T)
# typeof(z$ordinal_level_uno)


#get rid of prop storage and think about int vs double - likely late for the eda section
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
          running_name <- paste(names(dataset)[input_vector[i]], '_other_numeric')
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


# trial_data <- trial_dataframe
# xb <- c('x', 'ordinal_level_uno' ) #, c('doubls', 2,5,6)
# z <- Nominal_Detect(xb, trial_data, F, T)
# typeof(z$x)

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
        running_name <- paste(names(dataset)[input_vector[i]], '_other_nominal')
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
        running_name <- paste(names(dataset)[input_vector[i]], '_other_nominal')
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

NLP_Convert <- function(input_vector, dataset){
  input_vector <- column_recog_vector('NLP', input_vector, dataset)
  for(i in 1:length(input_vector)){
    dataset[,input_vector[i]] <- as.character(dataset[,input_vector[i]])
  }
  return(dataset)
}


# trial_data <- trial_dataframe
# xb <- c('x', 'ordinal_level_uno' ) #, c('doubls', 2,5,6)
# z <- data_type_detect(trial_data)
# typeof(z$x)

data_type_detect <- function(dataset,
                             NLP_force = c(),
                             ordinal_force = list(), #list(c(‘colname’, ‘level1’, ‘level2’))
                             nominal_force = c(),
                             numeric_force = c(),
                             alternate_nas = list(), #list(c(“colname1”, 0, 99),c(“colname2”, “hold”))
                             preserve_nonconform = T){
  #check input format
  if(!is.data.frame(dataset)){stop('Please pass a dataframe type structure to the function')}


  #initiate_vectors

  #determine original data type
  original_typeb <- sapply(dataset, class)
  original_names <- names(dataset)
  original_type <- original_typeb
  original_type[original_typeb == "factor"] <- 'Nominal'
  original_type[original_typeb == "logical"] <- 'Nominal'
  original_type[original_typeb == "numeric"] <- 'Float'
  original_type[original_typeb == "integer"] <- 'Integer'
  original_type[original_typeb == "character"] <- 'NLP'

  #remove alternate NAs
  dataset <- Alternate_NA_Remove(alternate_nas, dataset)

  #determine forced columns
  if(length(NLP_force) >= 1){
    NLP_force <- column_recog_vector('NLP', NLP_force, dataset)
  }
  if(length(nominal_force) >= 1){
    nominal_force <- column_recog_vector('nominal', nominal_force, dataset)
  }
  if(length(numeric_force) >= 1){
    numeric_force <- column_recog_vector('numeric', numeric_force, dataset)
  }
  if(length(ordinal_force) >= 1){
    ordinal_force <- column_recog_list('ordinal', ordinal_force, dataset)
  }




  if(length(ordinal_force) >= 1){
    forced_columns <- c(NLP_force, nominal_force, numeric_force, ordinal_force[[1]])
  }
  else{
    forced_columns <- c(NLP_force, nominal_force, numeric_force)
  }
  if(any(duplicated(forced_columns))){
    stop(paste('Column', forced_columns[duplicated(forced_columns)], 'has been forced to multiple data types, please reconsider'))
  }
  #auto detect columns are the remaining ones
  if(length(forced_columns) >= 1){
    columns_to_detect <- (1:ncol(dataset))[-forced_columns]
  }
  else{
    columns_to_detect <- (1:ncol(dataset))
  }

  #force columns
#!TODO repeats the column recog function - look at streamlining
  if(length(ordinal_force) >= 1){
    dataset <- Ordinal_Force(ordinal_force, dataset, preserve_nonconform = preserve_nonconform)
  }


  #keep records of int vs double
  double_forced <- ints_forced <- c()
  if(length(numeric_force) >= 1){
    numeric_forced <- Numeric_Type_Detect(numeric_force, dataset, preserve_nonconform = preserve_nonconform, force = T)
    dataset <- numeric_forced$data
    double_forced <- numeric_forced$floats
    ints_forced <- numeric_forced$integers
    rm(numeric_forced)
  }


  if(length(nominal_force) >= 1){
    dataset <- Nominal_Detect(nominal_force, dataset, preserve_nonconform = preserve_nonconform, force = T)
  }


  if(length(NLP_force) >= 1){
    dataset <- NLP_Convert(NLP_force, dataset)
  }


  #detect columns
  if(length(columns_to_detect) >= 1){
    #categorical first as stricter on classifying
    cat_detect <- Nominal_Detect(columns_to_detect, dataset, preserve_nonconform = preserve_nonconform, force = F)
    dataset <- cat_detect$data
    cats_detected <- cat_detect$detected
    columns_to_detect <- columns_to_detect[!(columns_to_detect %in% cats_detected)]
  }
  if(length(columns_to_detect) >= 1){
    #numeric_data
    numeric_detect <- Numeric_Type_Detect(columns_to_detect, dataset, preserve_nonconform = preserve_nonconform, force = F)
    dataset <- numeric_detect$data
    double_detected <- numeric_detect$floats
    ints_detected <- numeric_detect$integers
    rm(numeric_detect)
    columns_to_detect <- columns_to_detect[!(columns_to_detect %in% c(double_detected, ints_detected))]
  }

  if(length(columns_to_detect) >= 1){
    #remainder columns set to NLP
    dataset <- NLP_Convert(columns_to_detect, dataset)
  }


  NLP_final <- names(dataset)[c(columns_to_detect, NLP_force)]
  Integer_final <- names(dataset)[c(ints_forced, ints_detected)]
  Floating_final <- names(dataset)[c(double_forced, double_detected)]
  Nominal_final <- names(dataset)[c(cats_detected, nominal_force)]
  Ordinal_final <- c()
  if(length(ordinal_force)>= 1){
    Ordinal_final <- names(dataset)[c(ordinal_force)]
  }


  converted_names<- original_names
  converted_names[original_names %in% NLP_final] <- 'NLP'
  converted_names[original_names %in% Integer_final] <-'Integer'
  converted_names[original_names %in% Floating_final] <- 'Float'
  converted_names[original_names %in% Nominal_final] <-'Nominal'
  converted_names[original_names %in% Ordinal_final] <-'Ordinal'

  output <- list(data = dataset,
                 original_type = data.frame(data_field = original_names, data_type = original_type),
                 converted_type = data.frame(data_field = original_names, data_type = converted_names))
}


#!TODO
# - shift output of detects if non forced to include which columns have been detected and then the data (list(vector, dataframe))
# - documentation
# - vignette
# - testing
# - testing plan

