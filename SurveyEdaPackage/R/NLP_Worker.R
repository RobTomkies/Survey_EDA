
convert_tidy <- function(column){
  return(tibble(response = 1:length(column), text = column))
}



reponse_word_count <- function(column){
  word_count <- sapply(gregexpr("[[:alpha:]]+", column), function(x) sum(x > 0))
  return(word_count)
}


tfidf_score <- function(column){
  dataset <- column %>%  unnest_tokens(word, text)%>%
    filter(!word %in% stop_words$word)

  dataset$word <- dataset$word %>%
    str_extract( "^[a-z0-9'._]*$")

  dataset <- dataset %>% filter_all(all_vars(!is.na(.))) %>%
    count(response, word, sort = TRUE)

  total_words <- dataset %>%
    summarize(total = sum(n))

  dataset <- dataset %>% mutate(total_words) %>%
    bind_tf_idf(word, response , n) %>%
    arrange(desc(tf_idf)) %>%
    select(response, word, tf_idf) %>%
    head(n = 500)

  return(dataset)
}




EDA_Word_cor_score <- function(column){

  dataset <- column %>%  unnest_tokens(word, text)%>%
    filter(!word %in% stop_words$word)

  dataset$word <- dataset$word %>%
    str_extract( "^[a-z0-9'._]*$")

  word_corrs <- dataset %>% filter_all(all_vars(!is.na(.))) %>%
    group_by(word) %>%
    filter(n() >= 15)

  if(length(word_corrs$word) <= 5){warning('fewer than 5 meaningful words in column, word corrolation analysis not possible')
    word_corrs <- NA}
  else{
    word_corrs<- word_corrs %>% pairwise_cor(word, response , sort = TRUE)%>%
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
    unnest_tokens(bigram, text, token = "ngrams", n = n) %>%
    separate(bigram, column_names, sep = " ") %>%
    select(all_of(column_names))
  dataset <- data.frame(lapply(dataset, function(x)return(x %>% str_extract("^[a-z0-9'._]*$"))))

  dataset <- filter_all(dataset, all_vars(!(. %in% stop_words$word)))%>%
    filter_all(all_vars(!is.na(.)))%>%
    count(across(), sort = TRUE) %>%
    head(n = 500)
  return(data.frame(dataset))
}
