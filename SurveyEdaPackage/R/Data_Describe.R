
#' @export
Data_Describe <- function(dataset,
                          NLP.force = c(),
                          ordinal.force = list(), #list(c(‘colname’, ‘level1’, ‘level2’))
                          nominal.force = c(),
                          numeric.force = c(),
                          alternate.nas = list(), #list(c(“colname1”, 0, 99),c(“colname2”, “hold”))
                          preserve.nonconform = T,
                          remove.repeat = T){
  #dataset dimensions
  original_nrow <- nrow(dataset)
  original_ncol <- ncol(dataset)


  #detect and correct for data types, includes many error catching steps.
  updated_data <- SurveyEdaPackage::data_type_detect(dataset, NLP_force = NLP.force, ordinal_force = ordinal.force,
                   nominal_force = nominal.force, numeric_force = numeric.force,
                   alternate_nas = alternate.nas,
                   preserve_nonconform = preserve.nonconform)
  #clean up original input data
  rm(dataset)

  #number of identicle rows
  if(remove.repeat == T){
    repeat_row_count <- nrow(updated_data$data)
    updated_data$data <- unique(updated_data$data)
    repeat_row_count <- repeat_row_count - nrow(updated_data$data)
  }
  else{
    repeat_row_count <- nrow(updated_data$data) - nrow(unique(updated_data$data))
  }

  #dimensions - new data
  updated_ncol <- ncol(updated_data$data)
  updated_nrow <- nrow(updated_data$data)

  #memory
  memory_size <- object.size(updated_data$data)




  #datatypes
  data_types <- cbind( updated_data$original_type['data_type'], updated_data$converted_type['data_type'])
  names(data_types) <- c('original type', 'converted type')

  #Data type lists
  Ordinal_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Ordinal']
  NLP_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Natural Language']
  Integer_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Integer']
  Float_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Float']
  Nominal_type <- updated_data$converted_type$data_field[updated_data$converted_type$data_type == 'Nominal']

  ML <- max(sapply(list(Ordinal_type, NLP_type, Integer_type, Float_type, Nominal_type), length))
  #pad vectors
  length(Ordinal_type) <- length(NLP_type)<- length(Integer_type)<- length(Float_type)<- length(Nominal_type) <- ML
  #bind
  dt_type_frame <- cbind(Ordinal_type, NLP_type, Integer_type, Float_type, Nominal_type)



  #missingness
  missingness_vector <- apply(updated_data$data, 2,  function(x){return(100*(sum(is.na(x)))/length(x))})
  #names
  complete <- names(missingness_vector)[missingness_vector == 0]
  u10 <- names(missingness_vector)[missingness_vector <= 10 & missingness_vector > 0]
  mt10_25 <- names(missingness_vector)[missingness_vector <= 25 & missingness_vector > 10]
  mt25_u50 <- names(missingness_vector)[missingness_vector <= 50 & missingness_vector > 25]
  mt50_u75 <- names(missingness_vector)[missingness_vector <= 75 & missingness_vector > 50]
  mt75 <- names(missingness_vector)[missingness_vector > 75]

  ML <- max(sapply(list(complete, u10,mt10_25, mt25_u50, mt50_u75, mt75), length))
  #pad vectors
  length(complete) <- length(u10)<- length(mt10_25)<- length(mt25_u50)<- length(mt50_u75)<- length(mt75) <- ML
  #bind
  missingness_groups <- data.frame(cbind(complete, u10,mt10_25, mt25_u50, mt50_u75, mt75))
  names(missingness_groups) <- c('Complete', '>0 to 10%', '>10 to 25%', '>25 to 50%', '>50 to 75%', '>75%' )

  output <- list(dimensions = data.frame(nrow = c(original = original_nrow , adjusted = updated_nrow),
                                         ncol = c(original = original_ncol , adjusted = updated_ncol)),
                 memory_size = memory_size,
                 data_type_conversion = data_types,
                 data_type_names = dt_type_frame,
                 missingness_percentage = missingness_vector,
                 grouped_missingness = missingness_groups,
                 repeat_rows = repeat_row_count,
                 data = updated_data$data
                 )
  class(output) <- 'data_describe'
  return(output)
}

#' @export
print.data_describe <- function(x){
  x <- unclass(x)
  pander(x$dimensions, caption = "Data Dimensions")

  general_stats <- data.frame(rbind(c(x$repeat_rows, x$memory_size)))
  names(general_stats) <- c('Identical Rows Count', 'Memory Size (B)')
  pander(general_stats, caption = "General Data Statistics")

  pander(x$data_type_conversion, caption = "Original and detected data type converted to")

  output_dt_mis <- x$grouped_missingness
  output_dt_mis[is.na(output_dt_mis)] <- ""
  pander(output_dt_mis, caption = "Data fields by Missingness (%)", split.cells = 12)
}

#' @export
plot.data_describe <- function(x){
  x <- unclass(x)
  data_type_table <- table(x$data_type_conversion['converted type'])
  lbls1 <- paste(names(data_type_table), "\n", data_type_table, sep="")

  df_data_type <- x$data_type_conversion %>%
    group_by(`converted type`) %>% # Variable to be transformed
    count() %>%
    ungroup() %>%
    mutate(perc = `n` / sum(`n`)) %>%
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))

  p_dtype <- ggplot2::ggplot(df_data_type, aes(x = "", y = perc, fill = `converted type`)) +
    geom_col() + theme_void()+
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    coord_polar(theta = "y") +ggtitle("Data Types Present")+
    theme(plot.title = element_text(hjust = 0.5))


  counts <- c(sum(!is.na(x$grouped_missingness['Complete'])),
                              sum(!is.na(x$grouped_missingness['>0 to 10%'])),
                              sum(!is.na(x$grouped_missingness['>10 to 25%'])),
                              sum(!is.na(x$grouped_missingness['>25 to 50%'])),
                              sum(!is.na(x$grouped_missingness['>50 to 75%'])),
                              sum(!is.na(x$grouped_missingness['>75%'])))
  category <- c('Complete', '>0 to 10%', '>10 to 25%', '>25 to 50%', '>50 to 75%', '>75%' )[]

  df_missing <- data.frame(counts, category) %>%
    filter(counts > 0) %>%
    mutate(perc = `counts` / sum(`counts`)) %>%
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))

  p_missing <- ggplot2::ggplot(df_missing, aes(x = "", y = perc, fill = category)) +
    geom_col() +
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    theme_void()+
    coord_polar(theta = "y") +ggtitle("Missingness of data")+
    theme(plot.title = element_text(hjust = 0.5))

  missing_graphic <- x$data %>%
    mutate(id = row_number()) %>%
    gather(-id, key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    ggplot(aes(key, id, fill = isna)) +
    geom_raster(alpha=0.8) +
    theme(axis.text.y = element_text(angle = 45)) +
    scale_fill_manual(name = "",
                      values = c('steelblue', 'tomato3'),
                      labels = c("Present", "Missing")) +
    labs(x = "Variable",
         y = "Row Number", title = "Missing values in rows") +
    coord_flip()

  ggarrange(ggarrange(p_missing, p_dtype, ncol = 2), missing_graphic, nrow = 2)

}


# plotting structure:
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

# missingness:
# https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
