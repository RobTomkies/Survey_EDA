#' Natural Language Type Forcing
#'
#' @param input_vectorvector ; containing the names of the columns (as strings) or indexes of columns, or a combination,  that should be forced to natural language data type
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#'
#' @return
#' @export
#'
#' @examples
NLP_Convert <- function(input_vector, dataset){
  input_vector <- column_recog_vector('NLP', input_vector, dataset)
  for(i in 1:length(input_vector)){
    dataset[,input_vector[i]] <- as.character(dataset[,input_vector[i]])
  }
  return(dataset)
}

# trial_data <- trial_dataframe
# xb <- c('x', 'ordinal_level_uno' ) #, c('doubls', 2,5,6)
# z <- Nominal_Detect(xb, trial_data, F, T)
# typeof(z$x)
