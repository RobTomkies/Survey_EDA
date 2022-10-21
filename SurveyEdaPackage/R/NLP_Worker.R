#' @export
convert_tidy <- function(column){
  return(tibble(response = 1:length(column), text = column))
}


#' @export
reponse_word_count <- function(column){
  word_count <- sapply(gregexpr("[[:alpha:]]+", datain$reviews), function(x) sum(x > 0))
  return(word_count)
}

#' @export
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


#' @export
EDA_Word_cor_score <- function(column){

  dataset <- column %>%  unnest_tokens(word, text)%>%
    filter(!word %in% stop_words$word)

  dataset$word <- dataset$word %>%
    str_extract( "^[a-z0-9'._]*$")

  word_corrs <- dataset %>% filter_all(all_vars(!is.na(.))) %>%
    group_by(word) %>%
    filter(n() >= 25) %>%
    pairwise_cor(word, response , sort = TRUE)%>%
    head(n = 1000)

  return(word_corrs)
}

#' @export
NLP_ngram  <- function(column, n){
  column_names <- rep(NA, n)
  #loops prime to be shifted to rcpp
  for(i in 1:n){
    column_names[i] <- paste('word', i, sep = '')
  }
  dataset <- column %>%
    unnest_tokens(bigram, text, token = "ngrams", n = n) %>%
    separate(bigram, column_names, sep = " ") %>%
    select(column_names)

  dataset <- data.frame(lapply(dataset, function(x)return(x %>% str_extract("^[a-z0-9'._]*$"))))


  dataset <- filter_all(dataset, all_vars(!(. %in% stop_words$word)))%>%
    filter_all(all_vars(!is.na(.)))%>%
    count(across(), sort = TRUE) %>%
    head(n = 500)
  return(data.frame(dataset))
}