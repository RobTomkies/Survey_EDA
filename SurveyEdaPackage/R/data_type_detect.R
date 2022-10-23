#'Data Type Detection and Forcing
#'
#' This function receives a dataframe and outputs the dataframe with corrected
#' data types. The user can force columns to a certain data type via specifying
#' manually or allow a number of tests on data characteristics to automatically
#' detect the likely data type. In the common case for survey data where users
#' can select categories or enter free text and the data is included in one
#' column, the free text can be split out and preserved in additional columns
#' that will be added to the dataset.
#'
#'@details
#' Categorical data detection where unforced is split in to two approaches:
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
#' Numeric data detection where unforced is split in to two stages:
#'
#' - Firstly whether the data is numeric or not by seeing if over 60% is numeric information.
#' - If this is so and the data is less than 20 records long, if over 90% of the data is integer based then overall we classify as integer
#' - If numeric and over 20 records long, if over 95% is integer then we classify as interger
#' - If either of the last two steps fail but still over 60% numeric information we classify as floating point numeric data

#'
#' Of course if this is ineffective at detecting columns can be manually forced.
#'
#' As ordinal data is almost impossible to automatically detect, this format is only implemented
#' when specified. Otherwise categorical data is detected and assumed as nominal.
#'
#' Natural Language data (NLP ) is either forced manually or assumed to be the absence of either
#' categorical (nominal or ordinal), numeric (integer or floating point).
#'
#'
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#' @param NLP_force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination,  that should be forced to natural language data type
#' @param ordinal_force list ; containing vectors for each column you wished forced to ordinal data type. The first element of each vector should be the name/index of the column you wish to force, followed by levels of the ordinal in order you wish them to be handles.
#' @param nominal_force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced to nominal data type
#' @param numeric_force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced to numeric data type.
#' @param alternate_nas list ; containing vectors for each column you wish to specify alternate/additional NA values for. The first element of each vector should be the name/index of the column you wish to force followed by the additional values in the column that should be considered as NA.
#' @param preserve_nonconform boolean ; value (T of F) on whether to separate and store data that does not conform to the detected/specified column type in an additional column
#'
#' @return A list containing:
#' -  data : The adjusted dataframe,
#' -  original_type: the original data type of the input dataframe
#' -  detected_type: the newly detected
#' @export
#' @examples
#' data_type_detect(dataset = example_dataset,
#'                  NLP_force = c(1,2,3),
#'                  ordinal_force = list(c('col1', 'a','b','c'), c('col2', 2,3,4)),
#'                  nominal_force = c('col5','col7'),
#'                  numeric_force = c(14, 'colb'),
#'                  alternate_nas = list(c('col1', 'error')),
#'                  preserve_nonconform = T)
#'
#' data_type_detect(example_dataset,
#'                  nominal_force = c('col5','col7'),
#'                  alternate_nas = list(c('col1', 'error')))

data_type_detect <- function(dataset,
                             NLP_force = c(),
                             ordinal_force = list(),
                             nominal_force = c(),
                             numeric_force = c(),
                             alternate_nas = list(),
                             preserve_nonconform = T){
  #check input format
  if(!is.data.frame(dataset)){stop('Please pass a dataframe type structure to the function')}
  if(!( is.logical(preserve_nonconform))){stop('Inputs for "preserve_nonconform"  must be logical True or False, please correct')}



  #initiate_vectors

  #determine original data type
  original_typeb <- sapply(dataset, class)
  original_names <- names(dataset)
  original_type <- original_typeb
  original_type[original_typeb == "factor"] <- 'Nominal'
  original_type[original_typeb == "logical"] <- 'Nominal'
  original_type[original_typeb == "numeric"] <- 'Float'
  original_type[original_typeb == "integer"] <- 'Integer'
  original_type[original_typeb == "character"] <- 'Natural Language'

  #remove alternate NAs
  dataset <- Alternate_NA_Remove(alternate_nas, dataset)

  #determine forced columns
  if(length(NLP_force) >= 1){
    NLP_force <- column_recog_vector('Natural Language', NLP_force, dataset)
  }

  if(length(nominal_force) >= 1){
    nominal_force <- column_recog_vector('nominal', nominal_force, dataset)
  }

  if(length(numeric_force) >= 1){
    numeric_force <- column_recog_vector('numeric', numeric_force, dataset)
  }
  if(length(ordinal_force) >= 1){
    ordinal_forceb <- column_recog_list('ordinal', ordinal_force, dataset)
    ordinal_force <- list()
    length(ordinal_force)<- length(ordinal_forceb[[1]])
    for(i in 1:length(ordinal_forceb[[1]])){
      ordinal_force[[i]] <- c(ordinal_forceb[[1]][i],ordinal_forceb[[2]][[i]] )
    }
    forced_columns <- c(NLP_force, nominal_force, numeric_force, ordinal_forceb[[1]])
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
    #remainder columns set to Natural Language
    dataset <- NLP_Convert(columns_to_detect, dataset)
  }


  NLP_final <- names(dataset)[c(columns_to_detect, NLP_force)]
  Integer_final <- if(exists('ints_detected')){names(dataset)[c(ints_forced, ints_detected)]}else{names(dataset)[c(ints_forced)]}
  Floating_final <- if(exists('double_detected')){names(dataset)[c(double_forced, double_detected)]}else{names(dataset)[c(double_forced)]}
  Nominal_final <- if(exists('cats_detected')){names(dataset)[c(cats_detected, nominal_force)]}else{names(dataset)[nominal_force]}

  Ordinal_final <- c()
  if(length(ordinal_force)>= 1){
    Ordinal_final <- names(dataset)[ordinal_forceb[[1]]]
  }


  converted_names<- original_names
  converted_names[original_names %in% NLP_final] <- 'Natural Language'
  converted_names[original_names %in% Integer_final] <-'Integer'
  converted_names[original_names %in% Floating_final] <- 'Float'
  converted_names[original_names %in% Nominal_final] <-'Nominal'
  converted_names[original_names %in% Ordinal_final] <-'Ordinal'


  output <- list(data = dataset,
                 original_type = data.frame(data_field = original_names, data_type = original_type),
                 converted_type = data.frame(data_field = original_names, data_type = converted_names))
  class(output) <- c("data_detected", class(output))
  return(output)
}

#' Print function for data_detected s3 object
#'
#' Prints the top 10 rows of the output data following running the data_type_detect() function
#'
#' @param x data_detected s3 object
#'
#' @export
print.data_detected <- function(x){
  x <- unclass(x)
  data <- x$data
  pander(head(data))
}


#' Summary function for data_detected s3 object
#'
#' Prints markdown friendly table of the input data format detected as well as the output format of data following running the data_type_detect() function
#'
#' @param x data_detected s3 object
#'
#' @export
summary.data_detected <- function(x){
  x <- unclass(x)
  data <- x$original_type
  data <- cbind(data_field = data[,1], original_type = data[,2], converted_type = x$converted_type[,2])
  pander(data)
}


