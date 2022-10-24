
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



Alternate_NA_Remove <- function(input_list, dataset){
  if(length(input_list) >= 1){
    adjusted_input_list <- column_recog_list('alternate_NA', input_list, dataset)
    column_indexes <- adjusted_input_list[[1]]
    NA_values <- adjusted_input_list[[2]]
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

