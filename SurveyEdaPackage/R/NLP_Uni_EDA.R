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


}


convert_tidy <- function(column){
  return(tibble(response = 1:length(column), text = column))
}



NLP_ngram  <- function(dataset, n){
  column_names <- rep(NA, n)
  #loops prime to be shifted to rcpp
  for(i in 1:n){
    column_names[i] <- paste('word', i, sep = '')
  }
  dataset <- dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = n) %>%
    separate(bigram, column_names, sep = " ") %>%
    select(column_names)

  dataset <- data.frame(lapply(dataset, function(x)return(x %>% str_extract("^[a-z0-9'._]*$"))))


  dataset <- filter_all(dataset, all_vars(!(. %in% stop_words$word)))%>%
    filter_all(all_vars(!is.na(.)))%>%
    count(across(), sort = TRUE)
  return(data.frame(dataset))
}
