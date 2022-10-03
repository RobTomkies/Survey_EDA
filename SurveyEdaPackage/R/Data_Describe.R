
#' @export
Data_Describe <- function(dataset,
                          NLP.force = c(),
                          ordinal.force = list(), #list(c(‘colname’, ‘level1’, ‘level2’))
                          nominal.force = c(),
                          numeric.force = c(),
                          alternate.nas = list(), #list(c(“colname1”, 0, 99),c(“colname2”, “hold”))
                          preserve.nonconform = T,
                          remove.repeat = T){
  #dataset dimensions
  original_nrow <- nrow(dataset)
  original_ncol <- ncol(dataset)


  #detect and correct for data types, includes many error catching steps.
  updated_data <- data_type_detect(dataset, NLP_force = NLP.force, ordinal_force = ordinal.force,
                   nominal_force = nominal.force, numeric_force = numeric.force,
                   alternate_nas = alternate.nas,
                   preserve_nonconform = preserve.nonconform)
  #clean up original input data
  rm(dataset)

  #dimensions - new data
  updated_ncol <- ncol(updated_data$data)
  updated_nrow <- nrow(updated_data$data)

  #memory
  memory_size <- object.size(updated_data$data)

  #number of identicle rows
  if(remove.repeat == T){
    updated_data <- dplyr::distinct(updated_data)
  }


  #datatypes
  data_types <- cbind( updated_data$original_type['data_type'], updated_data$converted_type['data_type'])
  names(data_types) <- c('original type', 'converted type')

  #Data type lists
  Ordinal_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Ordinal']
  NLP_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'NLP']
  Integer_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Integer']
  Float_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Float']
  Nominal_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Nominal']

  ML <- max(sapply(list(Ordinal_type, NLP_type, Integer_type, Float_type, Nominal_type), length))
  #pad vectors
  length(Ordinal_type) <- length(NLP_type)<- length(Integer_type)<- length(Float_type)<- length(Nominal_type) <- ML
  #bind
  dt_type_frame <- cbind(Ordinal_type, NLP_type, Integer_type, Float_type, Nominal_type)



  #missingness
  missingness_vector <- apply(updated_data$data, 2,  function(x){return(100*(sum(is.na(x)))/length(x))})
  #names
  complete <- names(missingness_vector)[missingness_vector == 0]
  u10 <- names(missingness_vector)[missingness_vector <= 10 & missingness_vector > 0]
  mt10_25 <- names(missingness_vector)[missingness_vector <= 25 & missingness_vector > 10]
  mt25_u50 <- names(missingness_vector)[missingness_vector <= 50 & missingness_vector > 25]
  mt50_u75 <- names(missingness_vector)[missingness_vector <= 75 & missingness_vector > 50]
  mt75 <- names(missingness_vector)[missingness_vector > 75]

  ML <- max(sapply(list(complete, u10,mt10_25, mt25_u50, mt50_u75, mt75), length))
  #pad vectors
  length(complete) <- length(u10)<- length(mt10_25)<- length(mt25_u50)<- length(mt50_u75)<- length(mt75) <- ML
  #bind
  missingness_groups <- cbind(complete, u10,mt10_25, mt25_u50, mt50_u75, mt75)

  output <- list(dimensions = data.frame(nrow = c(original = original_nrow , adjusted = updated_nrow),
                                         ncol = c(original = original_ncol , adjusted = updated_ncol)),
                 memory_size = memory_size,
                 data_type_conversion = data_types,
                 data_type_names = dt_type_frame,
                 missingness_percentage = missingness_vector,
                 grouped_missingness = missingness_groups,
                 data = updated_data$data
                 )
  class(output) <- 'data_describe'
  return(output)
}

#' @export
print.data_describe <- function(x){
  x <- unclass(x)
  pander(x$dimensions)

  output_dt_type <- x$data_type_names
  output_dt_type[is.na(output_dt_type)] <- ""
  pander(output_dt_type)

  output_dt_mis <- x$grouped_missingness
  output_dt_mis[is.na(output_dt_mis)] <- ""
  pander(output_dt_mis)
}
