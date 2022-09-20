trial_dataframe <- data.frame(x = c(1,2,3,4,5,6,7,8), y = c(2,3,4,6,7,8,9,5), words = c('hello', 'my', 'name', 'is', 'rob', 'the', 'great', 'est'))

data_type_detect <- function(dataset,
                             NLP_force = c(),
                             ordinal_force = c(),
                             nominal_force = list(), #list(c(‘colname’, ‘level1’, ‘level2’))
                             numeric_force = c(),
                             date_force = c(),
                             alternate_nas = list(), #list(c(0, “colname1”),c(“hold”, “colname2”))
                             replace_nas = F){
  if(typeof(dataset) != "list"){stop('Please pass a dataframe type structure to the function')}
}

data_type_detect(data = trial_dataframe)
