

#' Data Describe
#'
#' Function calculates key measures such as the size, dimensions, memory requirements as well as more specific measure such as missingness and repeat row counts. These outputs can be printed and plotted in markdown friendly versions.
#'
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#' @param NLP.force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced and analysed as natural language (NLP) data type
#' @param ordinal.force list ; containing vectors for each column you wished forced and analysed as ordinal data type. The first element of each vector should be the name/index of the column you wish to force, followed by levels of the ordinal data in order you wish them to be handled.
#' @param nominal.force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced and analysed as nominal data type
#' @param numeric.force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced and analysed as numeric data type
#' @param alternate.nas list ; containing vectors for each column you wish to specify alternate/additional NA values for. The first element of each vector should be the name/index of the column you wish to force followed by the additional values in the column that should be considered as NA.
#' @param preserve.nonconform boolean; Default T - true of false as to whether to analyse potentially natuaral language answers within other data type columns and to split these answers in to a separate column
#' @param remove.repeat boolean;  Default T - true or false as to whether to remove repeat rows within the dataset
#'
#' @return
#' A list containing summary statistics about the dataset as a whole. These include
#' -  dimensions: dataframe; dimensions for the original input dataset as well as the adjusted output dataset
#' -  memory_size: numeric; the memory size in bites that the dataset takes up
#' -  data_type_conversion: dataframe; Contains each datafield, the original input data type as well as the detected/output datatype
#' -  data_type_names: dataframe; a dataframe containing a column per datatype and the columns containing names of the fields of that datatype
#' -  missingness_percentage: vector; A vector containing the percetage of missing data foudn in each data field
#' -  grouped_missingness: dataframe; Dataframe containing columns of the names of each data field with columns split in to complete, 0-10%, 10-25%, 25-50%, 50-75% and >75% missing data
#' -  repeat_rows: numeric; the number of repeat rows found in the dataset
#' -  data: dataframe; the output adjusted data.
#' @export
#'
#'@details
#'
#' For details on auto detection of data type see documentation for data_type_detect() function.
#'
#' @examples
#' Data_Describe(dataset = example_dataset,
#'                     NLP.force = list(c('col1', 'a','b','c'), c('col2', 2,3,4)),
#'                     ordinal_force = list(c('col3', 'a','b','c'), c('col4', 2,3,4)),
#'                     nominal_force = c('col5','col6'),
#'                     numeric.force = c('col7','col8'),
#'                     preserve.nonconform = T,
#'                     alternate.nas = list(c('col1', 'error')),
#'                     remove.repeat = T)
#'
#' Data_Describe(example_dataset)
#'
#'
#' plot(Data_Describe(example_dataset))
#'
#' print(Data_Describe(example_dataset))
#'
Data_Describe <- function(dataset,
                          NLP.force = c(),
                          ordinal.force = list(), #list(c(‘colname’, ‘level1’, ‘level2’))
                          nominal.force = c(),
                          numeric.force = c(),
                          alternate.nas = list(), #list(c(“colname1”, 0, 99),c(“colname2”, “hold”))
                          preserve.nonconform = T,
                          remove.repeat = T){

  if(!(is.logical(remove.repeat) & is.logical(preserve.nonconform))){stop('Inputs for "preserve.nonconform" and "remove.repeat" must be logical True or False, please correct')}

  #dataset dimensions
  original_nrow <- nrow(dataset)
  original_ncol <- ncol(dataset)


  #detect and correct for data types, includes many error catching steps.
  updated_data <- SurveyEdaPackage::data_type_detect(dataset, NLP_force = NLP.force, ordinal_force = ordinal.force,
                   nominal_force = nominal.force, numeric_force = numeric.force,
                   alternate_nas = alternate.nas,
                   preserve_nonconform = preserve.nonconform)
  #clean up original input data
  rm(dataset)

  #number of identicle rows
  if(remove.repeat == T){
    repeat_row_count <- nrow(updated_data$data)
    updated_data$data <- unique(updated_data$data)
    repeat_row_count <- repeat_row_count - nrow(updated_data$data)
  }
  else{
    repeat_row_count <- nrow(updated_data$data) - nrow(unique(updated_data$data))
  }

  #dimensions - new data
  updated_ncol <- ncol(updated_data$data)
  updated_nrow <- nrow(updated_data$data)

  #memory
  memory_size <- object.size(updated_data$data)




  #datatypes
  data_types <- cbind( updated_data$original_type['data_type'], updated_data$converted_type['data_type'])
  names(data_types) <- c('original type', 'converted type')

  #Data type lists
  Ordinal_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Ordinal']
  NLP_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Natural Language']
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
  missingness_groups <- data.frame(cbind(complete, u10,mt10_25, mt25_u50, mt50_u75, mt75))
  names(missingness_groups) <- c('Complete', '>0 to 10%', '>10 to 25%', '>25 to 50%', '>50 to 75%', '>75%' )

  output <- list(dimensions = data.frame(nrow = c(original = original_nrow , adjusted = updated_nrow),
                                         ncol = c(original = original_ncol , adjusted = updated_ncol)),
                 memory_size = memory_size,
                 data_type_conversion = data_types,
                 data_type_names = dt_type_frame,
                 missingness_percentage = missingness_vector,
                 grouped_missingness = missingness_groups,
                 repeat_rows = repeat_row_count,
                 data = updated_data$data
                 )
  class(output) <- 'data_describe'
  return(output)
}

#' Print method for data_describe s3 class
#'
#' Prints dimensions, general statistics and missingness groups for data on markdown format
#'
#' @param x data_describe s3 object
#'
#' @export
print.data_describe <- function(x){
  pander::panderOptions('knitr.auto.asis', FALSE)
  x <- unclass(x)
  pander::pander(x$dimensions, caption = "Data Dimensions")

  general_stats <- data.frame(rbind(c(x$repeat_rows, x$memory_size)))
  names(general_stats) <- c('Identical Rows Count', 'Memory Size (B)')
  pander::pander(general_stats, caption = "General Data Statistics")

  pander::pander(x$data_type_conversion, caption = "Original and detected data type converted to")

  output_dt_mis <- x$grouped_missingness
  output_dt_mis[is.na(output_dt_mis)] <- ""
  pander::pander(output_dt_mis, caption = "Data fields by Missingness (%)", split.cells = 12)
}

#' Plot method for data_describe s3 class
#'
#' Plots pie charts for missingness and data types present in data as well as a missingness plot showing the distribution of missing data throughout the dataset
#'
#' @param x data_describe s3 object
#'
#' @export
plot.data_describe <- function(x){
  x <- unclass(x)
  data_type_table <- table(x$data_type_conversion['converted type'])
  lbls1 <- paste(names(data_type_table), "\n", data_type_table, sep="")

  df_data_type <- x$data_type_conversion %>%
    dplyr::group_by(`converted type`) %>% # Variable to be transformed
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = `n` / sum(`n`)) %>%
    dplyr::arrange(perc) %>%
    dplyr::mutate(labels = scales::percent(perc))

  p_dtype <- ggplot2::ggplot(df_data_type, aes(x = "", y = perc, fill = `converted type`)) +
    ggplot2::geom_col() + ggplot2::theme_void()+
    ggplot2::geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    ggplot2::coord_polar(theta = "y") +ggtitle("Data Types Present")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5))


  counts <- c(sum(!is.na(x$grouped_missingness['Complete'])),
                              sum(!is.na(x$grouped_missingness['>0 to 10%'])),
                              sum(!is.na(x$grouped_missingness['>10 to 25%'])),
                              sum(!is.na(x$grouped_missingness['>25 to 50%'])),
                              sum(!is.na(x$grouped_missingness['>50 to 75%'])),
                              sum(!is.na(x$grouped_missingness['>75%'])))
  category <- c('Complete', '>0 to 10%', '>10 to 25%', '>25 to 50%', '>50 to 75%', '>75%' )[]

  df_missing <- data.frame(counts, category) %>%
    dplyr::filter(counts > 0) %>%
    dplyr::mutate(perc = `counts` / sum(`counts`)) %>%
    dplyr::arrange(perc) %>%
    dplyr::mutate(labels = scales::percent(perc))

  p_missing <- ggplot2::ggplot(df_missing, aes(x = "", y = perc, fill = category)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    ggplot2::theme_void()+
    ggplot2::coord_polar(theta = "y") +ggtitle("Missingness of data")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5))

  missing_graphic <- x$data %>%
    dplyr::mutate(id = row_number()) %>%
    tidyr::gather(-id, key = "key", value = "val") %>%
    dplyr::mutate(isna = is.na(val)) %>%
    ggplot2::ggplot(aes(key, id, fill = isna)) +
    geom_raster(alpha=0.8) +
    ggplot2::theme(axis.text.y = element_text(angle = 45)) +
    ggplot2::scale_fill_manual(name = "",
                      values = c('steelblue', 'tomato3'),
                      labels = c("Present", "Missing")) +
    ggplot2::labs(x = "Variable",
         y = "Row Number", title = "Missing values in rows") +
    ggplot2::coord_flip()

  ggpubr::ggarrange(ggpubr::ggarrange(p_missing, p_dtype, ncol = 2), missing_graphic, nrow = 2)

}



