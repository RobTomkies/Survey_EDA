

#' Univariate Numeric Exploratory Data Analysis
#'
#' This function receives a dataframe containing survey data with one column
#' for each question and will output summary and characteristic values for the
#' numeric data. Analysis is split in to integer and continuous types. Columns
#' can either be automatically detected or forced by the user for analysis.
#'
#' @param dataset dataset dataframe ; containing columns of each field and rows containing each record
#' @param detect Boolean ; True or False value on where the user wishes the function to automatically detect additional categorical fields, if false will only use the forced columns
#' @param numeric.force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced and analysed as numeric data type
#' @param ignore.columns vector ; the names of the columns (as strings) or indexes of columns, or a combination, that should be ignored during analysis
#' @param alternate.nas list ; containing vectors for each column you wish to specify alternate/additional NA values for. The first element of each vector should be the name/index of the column you wish to force followed by the additional values in the column that should be considered as NA.
#'
#' @return
#' List containing:
#' 1. Integer_characteristics - A list containing:
#'   - Means: vector; Mean of each integer field
#'   - Medians: vector; Medians of each integer field
#'   - Modes: list; list of a vector per field containing the modal integer values for each field
#'   - Maxs: vector; max of each integer field
#'   - Mins: vector; Minimum of each integer field
#'   - I.Q.Rs: vector; Inter quartile range of each integer field
#'   - S.Ds: vector; standard deviation of each integer field
#'   - Skews: vector; pearson second coefficient of skewness value for each field
#' 2. Float_Characteristics: list; A list containing the same analysis as above however for the continuous numeric data fields
#' 3. Integer_Data: dataframe; containing the data for the integer fields analysed
#' 4. Double_Data: dataframe; containing the data for the continuous numeric fields analysed
#' 5. GT_20pcnt_Flag: list; A list containing a vector for each field analysed where the vector contains the names of each value that makes up over 20% of the values seen, a potential indicator of either an alternate NA value or what is actually categorical data
#'
#' @export
#'
#' @examples
#' Numeric_Uni_EDA(dataset = example_dataset,
#'                     numeric.force = c('col5','col7'),
#'                     ignore.columns = c('col4'),
#'                     alternate.nas = list(c('col1', 'error'))
#'
#' Numeric_Uni_EDA(example_dataset)
#'
#' plot(Numeric_Uni_EDA(example_dataset))
#'
#' print(Numeric_Uni_EDA(example_dataset))
#'
#' summary(Numeric_Uni_EDA(example_dataset))
#'
Numeric_Uni_EDA <- function(dataset,
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


  drop_vector <- column_recog_vector('ignore.columns', ignore.columns, dataset)


  #separate int data by selecting integer column and
  #select the names where data type is equal to integer etc
  int_names_all <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Integer']
  int_names <- int_names_all[!(column_recog_vector('ignore.columns',int_names_all, dataset) %in% drop_vector)]
  integer_data <- updated_data$data %>% dplyr::select(all_of(int_names))

  double_names_all <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Float']
  double_names <- double_names_all[!(column_recog_vector('ignore.columns',double_names_all, dataset) %in% drop_vector)]
  double_data <- updated_data$data %>% dplyr::select(all_of(double_names))
  rm(updated_data)

  if(ncol(integer_data) == 0 | ncol(double_data) == 0 ){stop('No numeric data either detected or forced, please correct')}

  #integer values
  #if hunting for rcpp these can all be converted
  int_means <- RCPPColMean(as.matrix(integer_data))
  int_medians <-RCPPColMedian(as.matrix(integer_data))
  int_modes <- lapply(integer_data, function(x)return(as.numeric(names(table(x))[table(x) == max(table(x))])))
  int_modes <- sapply(int_modes, function(x){if(length(x)>10){return('More than 10 values')}else{return(x)}})
  int_max <-RCPPColMax(as.matrix(integer_data))
  int_min <-RCPPColMin(as.matrix(integer_data))
  int_IQR <- sapply(integer_data, IQR, na.rm = T)
  int_SD <-RCPPColSD(as.matrix(integer_data))

  #pearson second coefficient of skewness
  int_skew <-RCPPColSkew(as.matrix(integer_data))



  int_output_char <- list(Means = int_means, Medians = int_medians, Modes = int_modes,
                          Maxs = int_max, Mins = int_min, I.Q.Rs = int_IQR, S.Ds = int_SD,
                          Skews = int_skew)

  float_means <- RCPPColMean(as.matrix(double_data))
  float_medians <-RCPPColMedian(as.matrix(double_data))
  float_modes <- lapply(double_data, function(x)return(as.numeric(names(table(x))[table(x) == max(table(x))])))
  float_modes <- sapply(float_modes, function(x){if(length(x)>10){return('More than 10 values')}else{return(x)}})
  float_max <-RCPPColMax(as.matrix(double_data))
  float_min <-RCPPColMin(as.matrix(double_data))
  float_IQR <- sapply(double_data, IQR, na.rm = T)
  float_SD <-RCPPColSD(as.matrix(double_data))
  #pearson second coefficient of skewness
  float_skew <-RCPPColSkew(as.matrix(double_data))

  float_output_char <- list(Means = float_means, Medians = float_medians, Modes = float_modes,
                          Maxs = float_max, Mins = float_min, I.Q.Rs = float_IQR, S.Ds = float_SD,
                          Skews = float_skew)



  #start of returning rows that take up greater than 20%
  gt20flag <- lapply(cbind(integer_data, double_data), function(x)return(names(table(x))[(table(x)/length(x)) >= 0.2]))

  outputs <- list(Integer_Characteristics = int_output_char, Float_Characteristics= float_output_char,
                  Interger_Data = integer_data, Double_Data = double_data, GT_20pcnt_Flag = gt20flag)
  class(outputs) <- 'Numeric_EDA'
  return(outputs)
}


#' Print function for Numeric_Uni_EDA S3 Object
#'
#'  s3 method to print summary tables showing which fields have been analysed as integer or float type data as well as any values flagged as being over 20% of the values present
#'
#'
#' @param x Numeric_EDA s3 object
#'
#' @return Markdown format summary tables showing which fields have been analysed as integer or float type data as well as any values flagged as being over 20% of the values present
#'
#' @examples
#' x <- Numeric_Uni_EDA(basic_test_data)
#' print(x)
#'
#' @export
print.Numeric_EDA <- function(x){
  panderOptions('knitr.auto.asis', FALSE)
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

#' Summary function for Numeric_Uni_EDA S3 Object
#'
#'  s3 method to show detailed summary tables showing the statistical characteristics of the data as calculated by the function
#'
#'
#' @param x Numeric_EDA s3 object
#'
#' @return Markdown format summary tables showing detailed summary tables showing the statistical characteristics of the data
#'
#' @examples
#' x <- Numeric_Uni_EDA(basic_test_data)
#' summary(x)
#'
#' @export
summary.Numeric_EDA <- function(x){
  panderOptions('knitr.auto.asis', FALSE)
  x <- unclass(x)
  integer_names <- names(x$Interger_Data)
  num_o_ints <- length(integer_names)
  float_names <- names(x$Double_Data)
  num_o_floats <- length(float_names)
  variables <- length(x$Integer_Characteristics)

  output_int_df <- data.frame(matrix(ncol = variables, nrow = num_o_ints))
  names(output_int_df) <- names(x$Integer_Characteristics)
  rownames(output_int_df) <- integer_names

  for(i in 1:num_o_ints){
    for(j in 1:variables)
    output_int_df[i,j] = sapply(x$Integer_Characteristics[[j]][i], paste, collapse = ", ")
  }

  output_float_df <- data.frame(matrix(ncol = variables, nrow = num_o_floats))
  names(output_float_df) <- names(x$Float_Characteristics)
  rownames(output_float_df) <- float_names

  for(i in 1:num_o_floats){
    for(j in 1:variables)
      output_float_df[i,j] = sapply(x$Float_Characteristics[[j]][i], paste, collapse = ", ")
  }

  pander(output_int_df, caption = 'Integer numeric data characteristics')

  pander(output_float_df, caption = 'Float numeric data characteristics')
}

#' Plot function for Numeric_Uni_EDA S3 Object
#'
#'  s3 method to plot histograms and boxplots for integer data and violin and boxplots for conitnuous numeric type data
#'
#'
#' @param x Numeric_EDA s3 object
#'
#' @return Histograms and boxplots for integer data and violin and boxplots for conitnuous numeric type data
#'
#' @examples
#' x <- Numeric_Uni_EDA(basic_test_data)
#' plot(x)
#'
#' @export
plot.Numeric_EDA <- function(x){
  x <- unclass(x)
  integer_names <- names(x$Interger_Data)
  num_o_ints <- length(integer_names)
  float_names <- names(x$Double_Data)
  num_o_floats <- length(float_names)

  #float_data
  violins <- ggplot(x$Double_Data %>% pivot_longer( everything()), aes(x=name, y=value)) +
    geom_violin(trim=TRUE, fill='#A4A4A4', color="darkred")+
    geom_boxplot(width=0.1) + theme_minimal()+
    labs(title="Float Type",x="Variable", y = "Value (Each Independent)")+
    facet_wrap(name ~ ., scales = "free", ncol= 1,strip.position="right") + coord_flip()

  hists <- ggplot(x$Interger_Data%>% pivot_longer( everything()), aes(x = value)) +theme_minimal()+
    geom_histogram(fill = "#A4A4A4", colour = "darkred", bins = 15) +  geom_boxplot(width=0.1, position= position_nudge(y=-.5)) +
    labs(title="Integer Type",x="Value (Each Independent)", y = "Variable") +
    facet_wrap(name ~ ., scales = "free", ncol= 1,strip.position="right")

  blank_hold <- ggplot() + theme_void()
  if(num_o_floats > num_o_ints){
    ggarrange(violins,
              ggarrange(hists, blank_hold, ncol =1, heights = c(num_o_ints * 2, (num_o_floats-num_o_ints)*2)),
              ncol = 2, heights =c(num_o_floats *2))
  }
  else{
    ggarrange(hists,
              ggarrange(violins, blank_hold, ncol =1, heights = c(num_o_floats * 2, (num_o_ints-num_o_floats)*2)),
              ncol = 2, heights =c(num_o_floats *2))
  }

}





