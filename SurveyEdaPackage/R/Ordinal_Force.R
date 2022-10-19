#' Ordinal Data Forcing
#'
#' @param input_list list ; containing vectors for each column you wished forced to ordinal data type. The first element of each vector should be the name/index of the column you wish to force, followed by levels of the nominal in order you wish them to be handles.
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#' @param preserve_nonconform boolean ; value (T of F) on whether to separate and store data that does not conform to the detected/specified column type in an additional column
#'
#' @return
#' @export
#'
#' @examples
#' Ordinal_Force(list(c('x', 1, 2)), trial_data)
#'
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
        running_name <- paste(names(dataset)[column_indexes[i]], '_other')
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


# trial_data <- trial_dataframe
# x <- list(c('ordinal_level_uno', 1,2,3), c('x', 1,2,3)) #, c('doubls', 2,5,6)
# z <- Ordinal_Force(x, trial_data)
# z$ordinal_level_uno
