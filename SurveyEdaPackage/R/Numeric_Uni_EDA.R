Numeric_Uni_EDA <- function(dataset,
                            detect = T,
                            numeric.force = c(),
                            ignore_columns = c(), #this will need to have error catching built in for inputs - look and modularising the current ones?
                            alternate_nas = list()){
  #dataset dimensions
  original_nrow <- nrow(dataset)
  original_ncol <- ncol(dataset)

  #detect and correct for data types, includes many error catching steps.
  updated_data <- SurveyEdaPackage::data_type_detect(dataset,
                                                     numeric_force = numeric.force,
                                                     alternate_nas = alternate.nas,
                                                     preserve_nonconform = F)

  #separate int data by selecting integer column and
  integer_data <- updated_data$data[,updated_data$data_type_names[,'Integer_type'][!is.na(updated_data$data_type_names[,'Integer_type'])]]
  double_data <- updated_data$data[,updated_data$data_type_names[,'Float_type'][!is.na(updated_data$data_type_names[,'Float_type'])]]
  rm(updated_data)

  #integer values
  #if hunting for rcpp these can all be converted
  int_means <- sapply(integer_data, mean, na.rm = T)
  int_medians <- sapply(integer_data, median, na.rm = T)
  int_modes <- lapply(integer_data, function(x)return(as.numeric(names(table(x))[table(x) == max(table(x))])))
  int_max <- sapply(integer_data, max, na.rm = T)
  int_min <- int_max <- sapply(integer_data, min, na.rm = T)
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
  float_min <- int_max <- sapply(double_data, min, na.rm = T)
  float_IQR <- sapply(double_data, IQR, na.rm = T)
  float_SD <- sapply(double_data, sd, na.rm = T)
  #pearson second coefficient of skewness
  float_skew <- unlist(lapply(double_data, function(x)return((3*(mean(x, na.rm = T) - median(x, na.rm = T)))/sd(x, na.rm = T))))

  float_output_char <- list(Means = float_means, Medians = float_medians, Modes = float_modes,
                          Maxs = float_max, Mins = float_min, I.Q.Rs = float_IQR, S.Ds = float_SD,
                          Skews = float_skew)



  #start of returning rows that take up greater than 20%
  lapply(integer_data, function(x)return(names(x)[table(x)/length(x) >= 0.2]))

  outputs <- list(Integer_Characteristics = int_output_char, FLoat_Characteristics= float_output_char,
                  Interger_Data = integer_data, Double_Data = double_data)
  class(outputs) <- 'Numeric_EDA'



}
