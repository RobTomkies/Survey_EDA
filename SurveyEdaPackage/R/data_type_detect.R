#trial_dataframe <- data.frame(x = c(1,2,3,4,5,6,7,8), y = c(2,3,4,6,7,8,9,5), doubls = c(1.1,2,3,4.1,5.2,4.6,7.1,9.7),actual_int = c('1','2','3','4','5','6','7','8'),words = c('hello', 'my', 'name', 'is', 'rob', 'the', 'great', 'est'))

#z <- c('x', 'y', 'doubls', 4)
#column_recog('integer', z,  trial_dataframe)



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
    for(i in 1:length(input_vector)){
      #integer as string and not in names
      if((!(input_vector[i] %in% names(dataset))) & (suppressWarnings(!is.na(as.integer(input_vector[i]))))){
        warning(paste('Name \"',input_vector[i],'\" not found for', force_type,' forcing so coerced to integer\n'))
        output_vector[i] <- as.integer(input_vector[i])
      }
      #numeric but not integer and string
      else if((!(input_vector[i] %in% names(dataset))) & (suppressWarnings(!is.na(as.numeric(input_vector[i]))))){
        warning(paste('Name \"',input_vector[i],'\" not found for', force_type,' forcing so coerced to integer\n'))
        output_vector[i] <- as.integer(input_vector[i])
      }
      #name of column used
      else if(input_vector[i] %in% names(dataset)){
        output_vector[i] <- which(names(dataset) == input_vector[i])
      }
      else(stop(paste('could not find column name ', input_vector[i], ' from ', force_type, ' forcing. Please reconsider\n')))
    }
  }
  if(any(duplicated(output_vector))){
    warning('type 2: duplicate columns for forcing found - simplified to only one')
    output_vector <- output_vector[!duplicated(output_vector)]
  }
  return(as.integer(output_vector))
}



data_type_detect <- function(dataset,
                             NLP_force = c(),
                             ordinal_force = c(),
                             nominal_force = list(), #list(c(‘colname’, ‘level1’, ‘level2’))
                             numeric_force = c(),
                             date_force = c(),
                             alternate_nas = list(), #list(c(“colname1”, 0),c(“colname2”, “hold”))
                             replace_nas = F){

  if(!is.data.frame(dataset)){stop('Please pass a dataframe type structure to the function')}

  # #detecting numeric
  # # 1 : int , 2: float , categorical:
  # col_number <- ncol(dataset)
  # data_type <- rep(NA,col_number)
  #
  #
  # which(names(trial_dataframe) == "y")
  # for(i in 1:col_number){
  #
  # }
}



