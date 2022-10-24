

#' Analysis of a Single Natural Language Vector
#'
#' @param column_in vector; A vector of character entries, each entry should be of natural language format data
#' @param input_name character; The name of the field being analysed
#'
#' @return
#' A list containing the following analysis:
#' -  Word_Frequency: dataframe; containing the top 500 most common words and their count
#' -  Bigrams: dataframe; containing the top 500 most common bigrams (word pairings) from the data and their frequency (order specific)
#' -  Trigrams: dataframe; containing the top 500 most commonly mentioned trigrams (three words mentioned consecutively) and their counts (order specific)
#' -  Corr_matrix: dataframe; containing the pearson frequency corrolation for words mentioned in the same response (order non specific)
#' -  Tf_idf: tibble; containing the word, which response and tf_idf score for the analysis. This analysis works better on longer response answers
#' -  wordcount: dataframe; contains the word count for each response
#' -  Sentiments: dataframe; dataframe containing scores for anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative and positive for each response. This is determined using the NRC sentiment dictionary.
#' -  intput_name: character; The name of the data field that the analysis refers to.
#'
#'
#' @export
#'
#' @examples
#'
#' NLP_Column_Analysis('Column1', 'Question_2')
#'
#' plot(NLP_Column_Analysis(c(1), 'Question_2'))
#' print(NLP_Column_Analysis(c(1), 'Question_2'))
#'
NLP_Column_Analysis <- function(column_in, input_name){
  if(typeof(input_name)!= 'character'){stop('column name must be a string')}
  if(!(is.vector(column_in) & !is.list(column_in))){stop('Input data must be a vector of strings, one row for each response')}
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

#' Plot function for NLP_Column_Analysis S3 Object
#'
#'  s3 method to plot Barplots for sentiment scores, word frequency, bigrams and Trigrams as well as a graph map displaying clusters of corrolated words
#'
#'
#' @param x NLP_Column_Analysis s3 object
#' @param cor_cut The corrolation score to cut off for connections when plotting - default set to 0.2
#'
#' @return Barplots for sentiment scores, word frequency, bigrams and Trigrams as well as a graph map displaying clusters of corrolated words
#'
#' @examples
#' x <- NLP_Column_Analysis(basic_test_data$words)
#' plot(x)
#'
#' @export
plot.NLP_Column_Analysis <- function(x, cor_cut = 0.2){
  x <- unclass(x)

  emotions <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust', 'negative', 'positive')
  sentiment_counts <- sapply(x$Sentiments, sum)
  plot_frame <- data.frame(Emotions = emotions, Sentiment_Score = sentiment_counts)

  sentiment_summary <- ggplot2::ggplot(plot_frame, aes(x = factor(Emotions, levels = Emotions), y = Sentiment_Score , fill=Sentiment_Score)) +
    ggplot2::geom_bar(stat = "identity", color="black")+ggplot2::theme_minimal() +
    ggplot2::labs(title="Cumulate Sentiment Score",x="Emotion", y = "Cumulative Score")+ ggplot2::theme(legend.position="none")+
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust=1)) +ggplot2::scale_fill_distiller(palette = "Greens")

  input_data <- head(x$Word_Frequency, n=10)
  word_frequency <- ggplot2::ggplot(input_data, aes(x = n, y = factor(word1, levels = rev(word1)),fill=n)) +
    ggplot2::geom_bar(stat = "identity", color="black")+
    ggplot2::theme_minimal()+
    ggplot2::scale_fill_distiller(palette = "Greens")+
    ggplot2::labs(title="Word Frequency (top 10)",x="Count", y = "Word")+ ggplot2::theme(legend.position="none")+
    ggplot2::theme(axis.text.y = element_text(angle = 45, vjust=-1))

  input_data <- head(x$Bigrams, n=10) %>%
    dplyr::mutate(Bigram = paste(word1, word2)) %>%
    dplyr::select(Bigram, n)
  Bigram_frequency <- ggplot2::ggplot(input_data, aes(x = n, y = factor(Bigram, levels = rev(Bigram)),fill=n)) +
    ggplot2::geom_bar(stat = "identity", color="black")+
    ggplot2::theme_minimal()+
    ggplot2::scale_fill_distiller(palette = "Greens")+
    ggplot2::labs(title="Bigram Frequency (top 10)",x="Count", y = "Bigram")+ ggplot2::theme(legend.position="none")+
    ggplot2::theme(axis.text.y = element_text(angle = 45, vjust=-1))

  input_data <- head(x$Trigrams, n=10) %>%
    dplyr::mutate(Trigram = paste(word1, word2, word3)) %>%
    dplyr::select(Trigram, n)
  Trigram_frequency <- ggplot2::ggplot(input_data, aes(x = n, y = factor(Trigram, levels = rev(Trigram)),fill=n)) +
    ggplot2::geom_bar(stat = "identity", color="black")+
    ggplot2::theme_minimal()+
    ggplot2::scale_fill_distiller(palette = "Greens")+
    ggplot2::labs(title="Trigram Frequency (top 10)",x="Count", y = "Trigram")+ ggplot2::theme(legend.position="none")+
    ggplot2::theme(axis.text.y = element_text(angle = 45, vjust=-1))


  suppressWarnings(if(is.na(x$Corr_matrix)){
    corrolation_plot <- ggplot2::ggplot() + ggplot2::theme_void()
  }
  else{
    corrolation_plot <- x$Corr_matrix %>%
      dplyr::filter(correlation > cor_cut) %>%
      igraph::graph_from_data_frame() %>%
      ggraph::ggraph(layout = "fr") +
      ggraph::geom_edge_link(show.legend = FALSE) +
      ggraph::geom_node_point(color = "lightgreen", size = 5) +
      ggraph::geom_node_text(aes(label = name), repel = TRUE) +
      ggplot2::theme_void()+
      ggplot2::labs(title=paste("Corrolation clusters >",cor_cut, sep =""))+ ggplot2::theme(legend.position="none")
  })


  graphics <- ggpubr::ggarrange(ggpubr::ggarrange(sentiment_summary, word_frequency, ncol = 2),
                        ggpubr::ggarrange(Bigram_frequency, Trigram_frequency, ncol =2),
                        ggpubr::ggarrange(corrolation_plot, ncol = 1),
                        heights = c(1,1,2),
                        nrow = 3)


  ggpubr::annotate_figure(graphics, top = text_grob(as.character(x$input_name),
                                            color = "black", face = "bold", size = 15))

}
