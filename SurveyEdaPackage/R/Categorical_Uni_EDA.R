#' @export
Categorical_Uni_EDA <- function(dataset,
                            detect = T,
                            ordinal.force = list(),
                            nominal.force = c(),
                            ignore.columns = c(), #this will need to have error catching built in for inputs - look and modularising the current ones?
                            alternate.nas = list()){
  #dataset dimensions
  original_nrow <- nrow(dataset)
  original_ncol <- ncol(dataset)

  #detect and correct for data types, includes many error catching steps.
  updated_data <- SurveyEdaPackage::data_type_detect(dataset,
                                                     ordinal_force = ordinal.force,
                                                     nominal_force = nominal.force,
                                                     alternate_nas = alternate.nas,
                                                     preserve_nonconform = F)

  #separate cat data by selecting nominal and ordinal column and
  #select the names where data type is equal to ordinal etc
  Ord_names_all <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Ordinal']
  Ord_names <- Ord_names_all[!(column_recog_vector('ignore.columns',Ord_names_all, dataset) %in% drop_vector)]
  Ord_data <- updated_data$data %>% dplyr::select(Ord_names)

  Nom_names_all <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Nominal']
  Nom_names <- Nom_names_all[!(column_recog_vector('ignore.columns',Nom_names_all, dataset) %in% drop_vector)]
  Nom_data <- updated_data$data %>% dplyr::select(Nom_names)
  rm(updated_data)





}
