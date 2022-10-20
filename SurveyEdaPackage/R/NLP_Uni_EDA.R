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

  return(NLP_data)


}

#' @export
NLP_Column_Analysis <- function(column_in, input_name){
  if(typeof(input_name)!= 'character'){stop('column name must be a string')}
  if(!is.vector(column_in)){stop('Input data must be a vector of strings, one row for each response')}
  ## Tidy Format##
  #convert to tidy format
  working_data <- convert_tidy(column_in)
  #word frequency
  Word_Frequency <- NLP_ngram(working_data, 1)
  #Bigrams
  Bigrams <- NLP_ngram(working_data, 2)
  #Trigrams
  Trigrams <- NLP_ngram(working_data, 3)
  #word corrolations
  Corr_matrix <- EDA_Word_cor_score(working_data)
  #tfidf
  tf_idf <- tfidf_score(working_data)
  ##Non Tidy Format##
  #wordcount
  wordcount <- reponse_word_count(column_in)
  #Sentiment analysis
  sentiments <- get_nrc_sentiment(column_in)
  names(sentiments) <- paste(input_name,"_",names(sentiments), sep = "")

  output <- list(Word_Frequency = Word_Frequency, Bigrams =Bigrams,
                 Trigrams = Trigrams, Corr_matrix = Corr_matrix,
                 Tf_idf = tf_idf, Wordcount = wordcount,
                 Sentiments = sentiments)

  class(output) <- 'NLP_Column_Analysis'
  return(output)
}

#' @export
plot.NLP_Column_Analysis <- function(x, cor_cut = 0.2){
  x <- unclass(x)

  emotions <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust', 'negative', 'positive')
  sentiment_counts <- sapply(x$Sentiments, sum)
  plot_frame <- data.frame(Emotions = emotions, Sentiment_Score = sentiment_counts)

  sentiment_summary <- ggplot(plot_frame, aes(x = factor(Emotions, levels = Emotions), y = Sentiment_Score , fill=Sentiment_Score)) +
    geom_bar(stat = "identity", color="black")+theme_minimal() +
    labs(title="Cumulate Sentiment Score",x="Emotion", y = "Cumulative Score")+ theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text = element_text(size = 18)) +scale_fill_distiller(palette = "Greens")

  input_data <- head(x$Word_Frequency, n=10)
  word_frequency <- ggplot(input_data, aes(x = n, y = factor(word1, levels = rev(word1)),fill=n)) +
    geom_bar(stat = "identity", color="black")+
    theme_minimal()+
    scale_fill_distiller(palette = "Greens")+
    labs(title="Word Frequency (top 10)",x="Count", y = "Word")+ theme(legend.position="none")+
    theme(axis.text.y = element_text(angle = 45, vjust=-1), axis.text = element_text(size = 18))

  input_data <- head(x$Bigrams, n=10) %>%
    mutate(Bigram = paste(word1, word2)) %>%
    select(Bigram, n)
  Bigram_frequency <- ggplot(input_data, aes(x = n, y = factor(Bigram, levels = rev(Bigram)),fill=n)) +
    geom_bar(stat = "identity", color="black")+
    theme_minimal()+
    scale_fill_distiller(palette = "Greens")+
    labs(title="Bigram Frequency (top 10)",x="Count", y = "Bigram")+ theme(legend.position="none")+
    theme(axis.text.y = element_text(angle = 45, vjust=-1), axis.text = element_text(size = 18))

  input_data <- head(x$Trigrams, n=10) %>%
    mutate(Trigram = paste(word1, word2, word3)) %>%
    select(Trigram, n)
  Trigram_frequency <- ggplot(input_data, aes(x = n, y = factor(Trigram, levels = rev(Trigram)),fill=n)) +
    geom_bar(stat = "identity", color="black")+
    theme_minimal()+
    scale_fill_distiller(palette = "Greens")+
    labs(title="Trigram Frequency (top 10)",x="Count", y = "Trigram")+ theme(legend.position="none")+
    theme(axis.text.y = element_text(angle = 45, vjust=-1), axis.text = element_text(size = 18))

  corrolation_plot <- x$Corr_matrix %>%
    filter(correlation > .2) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(show.legend = FALSE) +
    geom_node_point(color = "lightgreen", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()+
    labs(title=paste("Corrolation clusters >",cor_cut, sep =""))+ theme(legend.position="none", axis.text = element_text(size = 18))

  ggarrange(ggarrange(sentiment_summary, word_frequency, ncol = 2),
            ggarrange(Bigram_frequency, Trigram_frequency, ncol =2),
            ggarrange(corrolation_plot, ncol = 1),
            heights = c(1,1,2),
            nrow = 3)

}


#' @export
convert_tidy <- function(column){
  return(tibble(response = 1:length(column), text = column))
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
reponse_word_count <- function(column){
  word_count <- sapply(gregexpr("[[:alpha:]]+", datain$reviews), function(x) sum(x > 0))
  return(word_count)
}

#TODO! wrapper, plotting, summary

