
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
                 Sentiments = sentiments, input_name = input_name)
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
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +scale_fill_distiller(palette = "Greens")

  input_data <- head(x$Word_Frequency, n=10)
  word_frequency <- ggplot(input_data, aes(x = n, y = factor(word1, levels = rev(word1)),fill=n)) +
    geom_bar(stat = "identity", color="black")+
    theme_minimal()+
    scale_fill_distiller(palette = "Greens")+
    labs(title="Word Frequency (top 10)",x="Count", y = "Word")+ theme(legend.position="none")+
    theme(axis.text.y = element_text(angle = 45, vjust=-1))

  input_data <- head(x$Bigrams, n=10) %>%
    mutate(Bigram = paste(word1, word2)) %>%
    select(Bigram, n)
  Bigram_frequency <- ggplot(input_data, aes(x = n, y = factor(Bigram, levels = rev(Bigram)),fill=n)) +
    geom_bar(stat = "identity", color="black")+
    theme_minimal()+
    scale_fill_distiller(palette = "Greens")+
    labs(title="Bigram Frequency (top 10)",x="Count", y = "Bigram")+ theme(legend.position="none")+
    theme(axis.text.y = element_text(angle = 45, vjust=-1))

  input_data <- head(x$Trigrams, n=10) %>%
    mutate(Trigram = paste(word1, word2, word3)) %>%
    select(Trigram, n)
  Trigram_frequency <- ggplot(input_data, aes(x = n, y = factor(Trigram, levels = rev(Trigram)),fill=n)) +
    geom_bar(stat = "identity", color="black")+
    theme_minimal()+
    scale_fill_distiller(palette = "Greens")+
    labs(title="Trigram Frequency (top 10)",x="Count", y = "Trigram")+ theme(legend.position="none")+
    theme(axis.text.y = element_text(angle = 45, vjust=-1))

  corrolation_plot <- x$Corr_matrix %>%
    filter(correlation > .2) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(show.legend = FALSE) +
    geom_node_point(color = "lightgreen", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()+
    labs(title=paste("Corrolation clusters >",cor_cut, sep =""))+ theme(legend.position="none")

  graphics <- ggarrange(ggarrange(sentiment_summary, word_frequency, ncol = 2),
                        ggarrange(Bigram_frequency, Trigram_frequency, ncol =2),
                        ggarrange(corrolation_plot, ncol = 1),
                        heights = c(1,1,2),
                        nrow = 3)


  annotate_figure(graphics, top = text_grob(as.character(x$input_name),
                                            color = "black", face = "bold", size = 15))

}
