#' Natural Language Type Forcing
#'
#' @param input_vector vector ; containing the names of the columns (as strings) or indexes of columns, or a combination,  that should be forced to natural language data type
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#'
#' @return dataset with coverted columns
#'
NLP_Convert <- function(input_vector, dataset){
  input_vector <- column_recog_vector('Natural Language', input_vector, dataset)
  for(i in 1:length(input_vector)){
    dataset[,input_vector[i]] <- as.character(dataset[,input_vector[i]])
  }
  return(dataset)
}


