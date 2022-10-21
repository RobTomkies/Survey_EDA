#' @export
NLP_Uni_EDA <- function(dataset,
                        detect = T, #need to adjust so it just forces or if includes detection
                        NLP.force = c(),
                        ignore.columns = c(),
                        analyse_split_answers = T,
                        alternate.nas = list()){

  #dataset dimensions
  original_nrow <- nrow(dataset)
  original_ncol <- ncol(dataset)



  updated_data <- SurveyEdaPackage::data_type_detect(dataset,
                                                     NLP_force = NLP.force,
                                                     alternate_nas = alternate.nas,
                                                     preserve_nonconform = analyse_split_answers)


  drop_vector <- column_recog_vector('ignore.columns', ignore.columns, dataset)

  drop_vector_names <- names(dataset)[drop_vector]
  drop_vector_names <- c(drop_vector_names, paste(drop_vector_names, ' _other', sep = ''))


  if(analyse_split_answers == T){
    split_out_columns <- names(updated_data$data)[!(names(updated_data$data) %in% updated_data$converted_type$data_field)]
    NLP_names_standard <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Natural Language']
    NLP_names <- c(NLP_names_standard, split_out_columns)
  }
  else{
    NLP_names <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Natural Language']
  }

  if(length(ignore.columns) >0 ){
    NLP_names <- NLP_names[!(NLP_names %in% drop_vector_names)]
  }


  if(length(NLP_names) > 0){
    NLP_data <- updated_data$data %>% dplyr::select(NLP_names)
  }else(stop('No NLP Data present to analyse'))

  output_data <- list()
  length(output_data) <- ncol(NLP_data)
  names(output_data)<- names(NLP_data)

  for(i in 1:ncol(NLP_data)){
    output_data[[i]] <- NLP_Column_Analysis(NLP_data[,i],names(NLP_data)[i])

  }

  class(output_data) <- 'aggregate_NLP'
  return(output_data)

}





#' @export
plot.aggregate_NLP <- function(x, cor_cut = 0.2){
  x <- unclass(x)
  for(i in 1:length(x)){
    print(plot(x[[i]], cor_cut = cor_cut))
  }
}

#' @export
print.aggregate_NLP <- function(x){
  x <- unclass(x)
  variables_analysed <- names(x)
  data_available <- c('Word Frequency', 'Bigrams', 'Trigrams', 'Correlation', 'Matrix', 'Tf-idf score', 'Wordcount', 'Sentiments')
  out_length <- max(length(variables_analysed), length(data_available))
  length(data_available) <- length(variables_analysed) <- out_length

  output_table <- data.frame(variables_analysed, data_available)
  names(output_table) <- c('Columns Analysed', 'Data available')
  output_table[is.na(output_table)] = ""

  pander(output_table)
}
