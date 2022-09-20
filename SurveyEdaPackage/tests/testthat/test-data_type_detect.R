#logrelative error
LRE <- function(x,c){
  if(c==0){return(round(-log(abs(x), base = 10)))}
  else{return(round(-log((abs(x-c)/c) , base = 10)))}
}

trial_dataframe <- data.frame(x = c(1,2,3,4,5,6,7,8), y = c(2,3,4,6,7,8,9,5), words = c('hello', 'my', 'name', 'is', 'rob', 'the', 'great', 'est'))

test_that("data input error catching", {
  expect_error(data_type_detect(2), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect(c(1,2,3,4,5,6)), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect('hello'), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect(matrix(data = c(1,2,3,45), nrow = 2)), "Please pass a dataframe type structure to the function")
  expect_silent(data_type_detect(trial_dataframe))
})
