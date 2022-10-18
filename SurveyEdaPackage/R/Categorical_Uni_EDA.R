#' @export
Categorical_Uni_EDA <- function(dataset,
                            detect = T,
                            ordinal.force = list(),
                            nominal.force = c(),
                            ignore.columns = c(), #this will need to have error catching built in for inputs - look and modularising the current ones?
                            alternate.nas = list()){

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
    Ord_names <- Ord_names_all[!(column_recog_vector('ignore.columns',Ord_names_all, dataset) %in% drop_vector)]
    Nom_names <- Nom_names_all[!(column_recog_vector('ignore.columns',Nom_names_all, dataset) %in% drop_vector)]
  }
  else{
    Ord_names <- Ord_names_all
    Nom_names <- Nom_names_all
  }
  if(length(Ord_names) > 0){
    Ord_data <- updated_data$data %>% dplyr::select(Ord_names)
    #probably can be shifted to rcpp
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
    Nom_data <- updated_data$data %>% dplyr::select(Nom_names)

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


#' @export
print.Categorical_EDA <- function(x){
  x <- unclass(x)
  if(!is.null(x$Nominal_Statistics[1])){
    #good spot to shift to rcpp
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

#' @export
plot.Categorical_EDA <- function(x){
  x <- unclass(x)



  if(!is.null(x$Ordinal_Data[1])){
    ord_names <- names(x$Ordinal_Data)
    num_o_ords <- length(ord_names)
    ggplot(x$Ordinal_Data %>% pivot_longer(everything())) +
      aes(x = name, fill = factor(value)) +
      geom_bar(position = "fill")+
      facet_wrap(name ~ ., scales = "free", ncol= 1,strip.position="right")

    ord_names <- names(x$Ordinal_Data)
    num_o_ords <- length(ord_names)
    data <- x$Ordinal_Data %>% pivot_longer(everything()) %>% group_by(value, name)%>%
      summarise(total_count=n(),.groups = 'drop') %>%
      mutate(prop = total_count / sum(total_count)) %>%
      filter(prop >= 0.05)%>%
      as.data.frame()
    data$value <- as.character(data$value)


    for(i in 1:length(unique(data$name))){
      working <- data %>% filter(name == unique(data$name)[i])
      other_prop <- 1 - sum(working$prop)
      data <- data %>% add_row(value = 'other <5%', name = as.character(unique(data$name)[i]), total_count = NA, prop = other_prop)%>%
        arrange(desc(prop))
    }
    #change to rcpp
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
    ggplot(new_dataframe) +
      aes(x = name, y = prop, fill = value, group=prop) +
      ggtitle("Ordinal Data")+
      geom_col()+
      geom_text(aes(y = label, label = value), vjust = 1., colour = "black") +
      facet_wrap(name ~ ., scales = "free", ncol= 5,strip.position="top")+
      theme(legend.position="none")
  }

  if(!is.null(x$Nominal_Data[1])){
    Nominal_names <- names(x$Nominal_Data)
    num_o_nums <- length(Nominal_names)
    data <- x$Nominal_Data %>% pivot_longer(everything()) %>% group_by(value, name)%>%
      summarise(total_count=n(),.groups = 'drop') %>%
      mutate(prop = total_count / sum(total_count)) %>%
      filter(prop >= 0.05)%>%
      as.data.frame()
    data$value <- as.character(data$value)


    for(i in 1:length(unique(data$name))){
      working <- data %>% filter(name == unique(data$name)[i])
      other_prop <- 1 - sum(working$prop)
      data <- data %>% add_row(value = 'other <5%', name = as.character(unique(data$name)[i]), total_count = NA, prop = other_prop)%>%
        arrange(desc(prop))
    }
    #change to rcpp
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
    ggplot(new_dataframe) +
      aes(x = name, y = prop, fill = value, group=prop) +
      ggtitle("Nominal Data")+
      geom_col()+
      geom_text(aes(y = label, label = value), vjust = 1., colour = "black") +
      facet_wrap(name ~ ., scales = "free", ncol= 5,strip.position="top")+
      theme(legend.position="none")
  }
}
