
#' Exporatory Analysis of Natural Language Data
#'
#' @param dataset dataframe ; containing columns of each field and rows containing each record
#' @param detect Boolean ; True or False value on where the user wishes the function to automatically detect additional categorical fields, if false will only use the forced columns
#' @param NLP.force vector ; containing the names of the columns (as strings) or indexes of columns, or a combination, that should be forced and analysed as natural language (NLP) data type
#' @param ignore.columns vector ; the names of the columns (as strings) or indexes of columns, or a combination, that should be ignored during analysis
#' @param analyse_split_answers boolean; true of false as to whether to analyse potentially natuaral language answers within other data type columns such as numeric or categorical
#' @param alternate.nas list ; containing vectors for each column you wish to specify alternate/additional NA values for. The first element of each vector should be the name/index of the column you wish to force followed by the additional values in the column that should be considered as NA.
#'
#' @return
#' A list containing one entry for each NLP field analysed. Each entry in the list contains the following analysis:
#' -  Word_Frequency: dataframe; containing the top 500 most common words and their count
#' -  Bigrams: dataframe; containing the top 500 most common bigrams (word pairings) from the data and their frequency (order specific)
#' -  Trigrams: dataframe; containing the top 500 most commonly mentioned trigrams (three words mentioned consecutively) and their counts (order specific)
#' -  Corr_matrix: dataframe; containing the pearson frequency corrolation for words mentioned in the same response (order non specific)
#' -  Tf_idf: tibble; containing the word, which response and tf_idf score for the analysis. This analysis works better on longer response answers
#' -  wordcount: dataframe; contains the word count for each response
#' -  Sentiments: dataframe; dataframe containing scores for anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative and positive for each response. This is determined using the NRC sentiment dictionary.
#' -  intput_name: character; The name of the data field that the analysis refers to.
#' @export
#'
#' @examples
#' NLP_Uni_EDA(dataset = example_dataset,
#'                     detect = T,
#'                     NLP.force = list(c('col1', 'a','b','c'), c('col2', 2,3,4)),
#'                     ignore.columns = c('col4'),
#'                     analyse_split_answers = T,
#'                     alternate.nas = list(c('col1', 'error'))
#'
#' NLP_Uni_EDA(example_dataset)
#'
#' plot(NLP_Uni_EDA(example_dataset))
#'
#' print(NLP_Uni_EDA(example_dataset))
#'
#'
#'
#'
NLP_Uni_EDA <- function(dataset,
                        detect = T, #need to adjust so it just forces or if includes detection
                        NLP.force = c(),
                        ignore.columns = c(),
                        analyse_split_answers = T,
                        alternate.nas = list()){

  if(!is.logical(analyse_split_answers)){stop('Input "analyse_split_answers" requires a logical True or False answer, please correct')}


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
    NLP_data <- updated_data$data %>% dplyr::select(all_of(NLP_names))
    NLP_data <- NLP_data %>%
      dplyr::mutate(across(everything(), as.character))
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





#' Plot function for NLP_Uni_EDA S3 Object
#'
#'  s3 method to plot Barplots for sentiment scores, word frequency, bigrams and Trigrams as well as a graph map displaying clusters of corrolated words
#'
#'
#' @param x aggregate_NLP s3 object
#' @param cor_cut The corrolation score to cut off for connections when plotting - default set to 0.2
#'
#' @return Barplots for sentiment scores, word frequency, bigrams and Trigrams as well as a graph map displaying clusters of corrolated words
#'
#' @examples
#' x <- NLP_Uni_EDA(basic_test_data)
#' plot(x)
#'
#' @export
plot.aggregate_NLP <- function(x, cor_cut = 0.2){
  x <- unclass(x)
  for(i in 1:length(x)){
    print(plot(x[[i]], cor_cut = cor_cut))
  }
}

#' Print function for NLP_Uni_EDA S3 Object
#'
#' Print function for aggregate_NLP s3 object which displays the fields that have been analysed and the data available for each.
#'
#' @param x  aggregate_NLP s3 object
#'
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

  pander::pander(output_table)
}
