
#' @export
Numeric_Uni_EDA <- function(dataset,
                            detect = T,
                            numeric.force = c(),
                            ignore.columns = c(), #this will need to have error catching built in for inputs - look and modularising the current ones?
                            alternate.nas = list()){
  #dataset dimensions
  original_nrow <- nrow(dataset)
  original_ncol <- ncol(dataset)

  #detect and correct for data types, includes many error catching steps.
  updated_data <- SurveyEdaPackage::data_type_detect(dataset,
                                                     numeric_force = numeric.force,
                                                     alternate_nas = alternate.nas,
                                                     preserve_nonconform = F)




  #separate int data by selecting integer column and
  #select the names where data type is equal to integer etc
  int_names <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Integer']
  integer_data <- updated_data$data %>% dplyr::select(int_names)
  double_names <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Float']
  double_data <- updated_data$data %>% dplyr::select(double_names)
  rm(updated_data)

  #integer values
  #if hunting for rcpp these can all be converted
  int_means <- sapply(integer_data, mean, na.rm = T)
  int_medians <- sapply(integer_data, median, na.rm = T)
  int_modes <- lapply(integer_data, function(x)return(as.numeric(names(table(x))[table(x) == max(table(x))])))
  int_max <- sapply(integer_data, max, na.rm = T)
  int_min <- sapply(integer_data, min, na.rm = T)
  int_IQR <- sapply(integer_data, IQR, na.rm = T)
  int_SD <- sapply(integer_data, sd, na.rm = T)
  #pearson second coefficient of skewness
  int_skew <- unlist(lapply(integer_data, function(x)return((3*(mean(x, na.rm = T) - median(x, na.rm = T)))/sd(x, na.rm = T))))

  int_output_char <- list(Means = int_means, Medians = int_medians, Modes = int_modes,
                          Maxs = int_max, Mins = int_min, I.Q.Rs = int_IQR, S.Ds = int_SD,
                          Skews = int_skew)

  #float values
  #if hunting for rcpp these can all be converted
  float_means <- sapply(double_data, mean, na.rm = T)
  float_medians <- sapply(double_data, median, na.rm = T)
  float_modes <- lapply(double_data, function(x)return(as.numeric(names(table(x))[table(x) == max(table(x))])))
  float_max <- sapply(double_data, max, na.rm = T)
  float_min <- sapply(double_data, min, na.rm = T)
  float_IQR <- sapply(double_data, IQR, na.rm = T)
  float_SD <- sapply(double_data, sd, na.rm = T)
  #pearson second coefficient of skewness
  float_skew <- unlist(lapply(double_data, function(x)return((3*(mean(x, na.rm = T) - median(x, na.rm = T)))/sd(x, na.rm = T))))

  float_output_char <- list(Means = float_means, Medians = float_medians, Modes = float_modes,
                          Maxs = float_max, Mins = float_min, I.Q.Rs = float_IQR, S.Ds = float_SD,
                          Skews = float_skew)



  #start of returning rows that take up greater than 20%
  gt20flag <- lapply(cbind(integer_data, double_data), function(x)return(names(table(x))[(table(x)/length(x)) >= 0.2]))

  outputs <- list(Integer_Characteristics = int_output_char, FLoat_Characteristics= float_output_char,
                  Interger_Data = integer_data, Double_Data = double_data, GT_20pcnt_Flag = gt20flag)
  class(outputs) <- 'Numeric_EDA'
  return(outputs)
}


#' @export
print.Numeric_EDA <- function(x){
  x <- unclass(x)
  integer_names <- names(x$Interger_Data)
  float_names <- names(x$Double_Data)
  output_length <- max(c(length(integer_names), length(float_names)))
  length(integer_names) <- length(float_names) <- output_length
  output_table <- cbind(float_names, integer_names)

  output_table[is.na(output_table)] <- ""
  pander(output_table, caption = 'Field name and numeric type included in analysis')

  max_repeat_length <- max(sapply(x$GT_20pcnt_Flag, length))
  high_repeat <- x$GT_20pcnt_Flag
  output_2 <- cbind(lapply(high_repeat, function(y){
    length(y) <- max_repeat_length
    return(y)
  }))
  output_2[is.na(output_2)] <- ""
  output_2[,1]<- unlist(lapply(output_2[,1], function(x){
    if(identical(x, character(0))){
      return('*NONE*')
    }

    else if(x == ""){
      return('*NONE*')
    }
    else{
      return(x)
    }
    } ))
  pander(output_2, caption = "Values that make up 20% or more of data in each field")
}

#' @export
summary.Numeric_EDA <- function(x){

}

#' @export
plot.Numeric_EDA <- function(x){

}


