
convert_tidy <- function(column){
  return(tibble(response = 1:length(column), text = column))
}



reponse_word_count <- function(column){
  word_count <- sapply(gregexpr("[[:alpha:]]+", column), function(x) sum(x > 0))
  return(word_count)
}


tfidf_score <- function(column){
  dataset <- column %>%  tidytext::unnest_tokens(word, text)%>%
    dplyr::filter(!word %in% tidytext::stop_words$word)

  dataset$word <- dataset$word %>%
    stringr::str_extract( "^[a-z0-9'._]*$")

  dataset <- dataset %>% dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>%
    dplyr::count(response, word, sort = TRUE)

  total_words <- dataset %>%
    dplyr::summarize(total = sum(n))

  dataset <- dataset %>% dplyr::mutate(total_words) %>%
    tidytext::bind_tf_idf(word, response , n) %>%
    dplyr::arrange(desc(tf_idf)) %>%
    dplyr::select(response, word, tf_idf) %>%
    head(n = 500)

  return(dataset)
}




EDA_Word_cor_score <- function(column){

  dataset <- column %>%  tidytext::unnest_tokens(word, text)%>%
    dplyr::filter(!word %in% tidytext::stop_words$word)

  dataset$word <- dataset$word %>%
    stringr::str_extract( "^[a-z0-9'._]*$")

  word_corrs <- dataset %>% dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>%
    dplyr::group_by(word) %>%
    dplyr::filter(n() >= 15)

  if(length(word_corrs$word) <= 5){warning('fewer than 5 meaningful words in column, word corrolation analysis not possible')
    word_corrs <- NA}
  else{
    word_corrs<- word_corrs %>% widyr::pairwise_cor(word, response , sort = TRUE)%>%
      head(n = 1000)
  }


  return(word_corrs)
}


NLP_ngram  <- function(column, n){
  column_names <- rep(NA, n)
  #loops prime to be shifted to rcpp
  for(i in 1:n){
    column_names[i] <- paste('word', i, sep = '')
  }
  dataset <- column %>%
    tidytext::unnest_tokens(bigram, text, token = "ngrams", n = n) %>%
    tidyr::separate(bigram, column_names, sep = " ") %>%
    dplyr::select(all_of(column_names))
  dataset <- data.frame(lapply(dataset, function(x)return(x %>% stringr::str_extract("^[a-z0-9'._]*$"))))

  dataset <- dplyr::filter_all(dataset, dplyr::all_vars(!(. %in% tidytext::stop_words$word)))%>%
    dplyr::filter_all(dplyr::all_vars(!is.na(.)))%>%
    dplyr::count(across(), sort = TRUE) %>%
    head(n = 500)
  return(data.frame(dataset))
}
