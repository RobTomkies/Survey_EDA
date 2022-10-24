#'Univariate Categorical Exploratory Data Analysis
#'
#' This function receives a dataframe containing survey data with one column
#' for each question and will output summary and characteristic values for the
#' categorical data. Analysis is split in to ordinal and nominal types. Columns
#' can either be automatically detected or forced by the user for analysis.
#'
#'
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#' @param detect Boolean ; True or False value on where the user wishes the function to automatically detect additional categorical fields, if false will only use the forced columns
#' @param ordinal.force list ; containing vectors for each column you wished forced and analysed as ordinal data type. The first element of each vector should be the name/index of the column you wish to force, followed by levels of the ordinal data in order you wish them to be handled.
#' @param nominal.force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced and analysed as nominal data type
#' @param ignore.columns vector ; the names of the columns (as strings) or indexes of columns, or a combination, that should be ignored during analysis
#' @param alternate.nas list ; containing vectors for each column you wish to specify alternate/additional NA values for. The first element of each vector should be the name/index of the column you wish to force followed by the additional values in the column that should be considered as NA.
#'
#' @return A list containing:
#' -  Nominal_Data : Adjusted nominal data that was analysed
#' -  Ordinal_Data: Adjusted ordinal data that was analysed
#' -  Ordinal_Statistics: List containing the most and least common categories along with their counts for ordinal data
#' -  Nominal_Statistics: List containing the most and least common categories along with their counts for nominal data
#' -  Nominal_Counts: List containing the counts for each category in the nominal data
#' -  Ordinal_Counts: List containing the counts for each category in the ordinal data
#'
#' @examples
#' Categorical_Uni_EDA(dataset = example_dataset,
#'                     ordinal_force = list(c('col1', 'a','b','c'), c('col2', 2,3,4)),
#'                     nominal_force = c('col5','col7'),
#'                     ignore.columns = c('col4'),
#'                     alternate.nas = list(c('col1', 'error'))
#'
#' Categorical_Uni_EDA(example_dataset)
#'
#' plot(Categorical_Uni_EDA(example_dataset))
#'
#' print(Categorical_Uni_EDA(example_dataset))
#'
#' @export
#'
Categorical_Uni_EDA <- function(dataset,
                            ordinal.force = list(),
                            nominal.force = c(),
                            ignore.columns = c(),
                            detect = T,
                            alternate.nas = list()){

  if(detect == F & length(ordinal.force) == 0 & length(nominal.force) == 0){stop('No categorical columns being analysed - correct inputs "detect", "ordinal.force" or "nominal.force"')}
  if(!(is.logical(detect))){stop('Inputs for "detect" must be logical True or False, please correct')}

  outputs <- list(Nominal_Data = NULL, Ordinal_Data = NULL,
                  Ordinal_Statistics = NULL, Nominal_Statistics = NULL,
                  Nominal_Counts = NULL, Ordinal_Counts = NULL)


  #dataset dimensions
  original_nrow <- nrow(dataset)
  original_ncol <- ncol(dataset)

  #detect and correct for data types, includes many error catching steps.
  updated_data <- SurveyEdaPackage::data_type_detect(dataset,
                                                     ordinal_force = ordinal.force,
                                                     nominal_force = nominal.force,
                                                     alternate_nas = alternate.nas,
                                                     preserve_nonconform = F)

  drop_vector <- column_recog_vector('ignore.columns', ignore.columns, dataset)

  #separate cat data by selecting nominal and ordinal column and
  #select the names where data type is equal to ordinal etc
  Ord_names_all <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Ordinal']
  Nom_names_all <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Nominal']

  if(length(ignore.columns) >0 ){
    if(length(Ord_names_all) > 0){Ord_names <- Ord_names_all[!(column_recog_vector('ignore.columns',Ord_names_all, dataset) %in% drop_vector)]}
    else{Ord_names <- Ord_names_all}
    if(length(Nom_names_all) > 0){Nom_names <- Nom_names_all[!(column_recog_vector('ignore.columns',Nom_names_all, dataset) %in% drop_vector)]}
    else{Nom_names <- Nom_names_all}
  }
  else{
    Ord_names <- Ord_names_all
    Nom_names <- Nom_names_all
  }
  if(length(Ord_names) == 0 & length(Nom_names) == 0){stop('No cetgorical data either detected or forced, please reconsider inputs')}

  if(length(Ord_names) > 0){
    Ord_data <- updated_data$data %>% dplyr::select(all_of(Ord_names))

    ordinal_stats <- lapply(Ord_data, function(x){
      counts <- sort(table(x),decreasing=TRUE)
      max_value <- counts[1]
      min_value <- counts[length(counts)]
      max_names <- names(counts)[unlist(counts) == max_value]
      min_names <- names(counts)[unlist(counts) == min_value]
      return(list(Max_Count = unname(max_value), Max_Categories = max_names,
                  Min_Count = unname(min_value), Min_Categories = min_names))
    })


    tables_ordinal <- sapply(Ord_data, table)

    outputs$Ordinal_Data <- Ord_data
    outputs$Ordinal_Statistics <- ordinal_stats
    outputs$Ordinal_Counts <- tables_ordinal
  }

  if(length(Nom_names) > 0){
    Nom_data <- updated_data$data %>% dplyr::select(all_of(Nom_names))

    nominal_stats <- lapply(Nom_data, function(x){
      counts <- sort(table(x),decreasing=TRUE)
      max_value <- counts[1]
      min_value <- counts[length(counts)]
      max_names <- names(counts)[unlist(counts) == max_value]
      min_names <- names(counts)[unlist(counts) == min_value]
      return(list(Max_Count = unname(max_value), Max_Categories = max_names,
                  Min_Count = unname(min_value), Min_Categories = min_names))
    })

    tables_nominal <- sapply(Nom_data, table)

    outputs$Nominal_Data <- Nom_data
    outputs$Nominal_Statistics <- nominal_stats
    outputs$Nominal_Counts <- tables_nominal
  }
  class(outputs) <- "Categorical_EDA"
  return(outputs)
}


#' Print function for Categorical_EDA S3 Object
#'
#'  s3 method to print summary tables for each of nominal and ordinal data displaying the most and least common categories and their respective counts
#'
#'
#' @param x Categorical_EDA s3 object
#'
#' @return Markdown format summary tables for each of nominal and ordinal data displaying the most and least common categories and their respective counts
#'
#' @examples
#' x <- Categorical_Uni_EDA(basic_test_data)
#' print(x)
#'
#' @export
print.Categorical_EDA <- function(x){
  x <- unclass(x)
  if(!is.null(x$Nominal_Statistics[1])){
    nom_data <- x$Nominal_Statistics
    nom_output <- data.frame(matrix(nrow= length(nom_data), ncol = 5))
    names(nom_output) <- c('Data Field', 'Most Common Count', 'Most Common Groups', 'Least Common Count', 'Least Common Groups')
    for(i in 1:length(nom_data)){
      nom_output[i,] <- c(names(nom_data)[i], nom_data[[i]]$Max_Count, paste(nom_data[[i]]$Max_Categories, collapse = ', '), nom_data[[i]]$Min_Count, paste(nom_data[[i]]$Min_Categories, collapse = ', '))
    }
    pander(nom_output[,c(1,2,3)], caption = 'Nominal Data Summary - Most Common')
    pander(nom_output[,c(1,4,5)], caption = 'Nominal Data Summary - Least Common')
  }

  if(!is.null(x$Ordinal_Statistics[1])){
    ord_data <- x$Ordinal_Statistics
    ord_output <- data.frame(matrix(nrow= length(ord_data), ncol = 5))
    names(ord_output) <- c('Data Field', 'Most Common Count', 'Most Common Groups', 'Least Common Count', 'Least Common Groups')
    for(i in 1:length(ord_data)){
      nom_output[i,] <- c(names(ord_data)[i], ord_data[[i]]$Max_Count, paste(ord_data[[i]]$Max_Categories, collapse = ', '), ord_data[[i]]$Min_Count, paste(ord_data[[i]]$Min_Categories, collapse = ', '))
    }
    pander(ord_output[,c(1,2,3)], caption = 'Ordinal Data Summary - Most Common')
    pander(ord_output[,c(1,4,5)], caption = 'Ordinal Data Summary - Least Common')
  }
}

#' Plot function for Categorical_EDA S3 Object
#'
#'  s3 method to plot proportional barplots of categorical data. Categories that make up less than 2% of the data are grouped together
#'
#'
#' @param x Categorical_EDA s3 object
#'
#' @return Proportional bar plots of each category with caetgories containing less than 2% of data grouped as other.
#'
#' @examples
#' x <- Categorical_Uni_EDA(basic_test_data)
#' plot(x)
#'
#' @export
plot.Categorical_EDA <- function(x){
  x <- unclass(x)

  if(!is.null(x$Ordinal_Data[1])){

    ord_names <- names(x$Ordinal_Data)
    num_o_ords <- length(ord_names)
    data <- x$Ordinal_Data %>% pivot_longer(everything()) %>% group_by(value, name)%>%
      summarise(total_count=n(),.groups = 'drop') %>%
      mutate(prop = total_count / sum(total_count)) %>%
      filter(prop >= 0.02)%>%
      as.data.frame()
    data$value <- as.character(data$value)


    for(i in 1:length(unique(data$name))){
      working <- data %>% filter(name == unique(data$name)[i])
      other_prop <- 1 - sum(working$prop)
      data <- data %>% add_row(value = 'other <2%', name = as.character(unique(data$name)[i]), total_count = NA, prop = other_prop)%>%
        arrange(desc(prop))
    }
    new_dataframe <- data.frame(matrix(nrow = nrow(data), ncol =ncol(data)+1 ))
    names(new_dataframe) <- c(names(data), 'label')
    row_count <- 0
    for(i in 1:length(unique(data$name))){
      working <- data %>% filter(name == unique(data$name)[i])
      temp_row <- nrow(working)
      working <- working %>% mutate(label = cumsum(prop))
      new_dataframe[(row_count+1):(row_count + temp_row),] <- working
      row_count <- row_count + temp_row
    }
    print(ggplot(new_dataframe) +
      aes(x = name, y = prop, fill = value, group=prop) +
      ggtitle("Ordinal Data")+
      geom_col()+
      geom_text(aes(y = label, label = value), vjust = 1., colour = "black") +
      facet_wrap(name ~ ., scales = "free", ncol= 5,strip.position="top")+
      theme(legend.position="none"))
  }

  if(!is.null(x$Nominal_Data[1])){
    Nominal_names <- names(x$Nominal_Data)
    num_o_nums <- length(Nominal_names)
    data <- x$Nominal_Data %>% pivot_longer(everything()) %>% group_by(value, name)%>%
      summarise(total_count=n(),.groups = 'drop') %>%
      mutate(prop = total_count / sum(total_count)) %>%
      filter(prop >= 0.02)%>%
      as.data.frame()
    data$value <- as.character(data$value)


    for(i in 1:length(unique(data$name))){
      working <- data %>% filter(name == unique(data$name)[i])
      other_prop <- 1 - sum(working$prop)
      data <- data %>% add_row(value = 'other <2%', name = as.character(unique(data$name)[i]), total_count = NA, prop = other_prop)%>%
        arrange(desc(prop))
    }
    new_dataframe <- data.frame(matrix(nrow = nrow(data), ncol =ncol(data)+1 ))
    names(new_dataframe) <- c(names(data), 'label')
    row_count <- 0
    for(i in 1:length(unique(data$name))){
      working <- data %>% filter(name == unique(data$name)[i])
      temp_row <- nrow(working)
      working <- working %>% mutate(label = cumsum(prop))
      new_dataframe[(row_count+1):(row_count + temp_row),] <- working
      row_count <- row_count + temp_row
    }
    print(ggplot(new_dataframe) +
      aes(x = name, y = prop, fill = value, group=prop) +
      ggtitle("Nominal Data")+
      geom_col()+
      geom_text(aes(y = label, label = value), vjust = 1., colour = "black") +
      facet_wrap(name ~ ., scales = "free", ncol= 5,strip.position="top")+
      theme(legend.position="none"))
  }
}
