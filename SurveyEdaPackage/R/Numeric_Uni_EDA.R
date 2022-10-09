Numeric_Uni_EDA <- function(dataset,
                            detect = T,
                            numeric.force = c(),
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




  outputs <- list(Interger_Data = integer_data, Double_Data = double_data)
  class(outputs) <- 'Numeric_EDA'



}
