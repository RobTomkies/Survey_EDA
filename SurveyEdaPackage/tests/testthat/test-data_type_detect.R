
#basic_test_data <- data.frame(x = c(1,2,3,4,3,1,2,2,2,1,4), y = c(2,3,4,6,7,8,9,5,8,4,1), doubls = c(1.1,2,3,4.1,5.2,4.6,7.1,9.7,3.4,4.3,2.1),actual_int = c('1','2','3','4','5','6','7','8', '10','9','99'),words = c('hello', 'my', 'name', 'is', 'rob', 'the', 'great', 'est', 'hero', 'ever', 'like'), ordinal_level_uno = c(1,2,3,2,3,1,'hello', 'dragon',1,2,1))
#use_data(basic_test_data)


#logrelative error
LRE <- function(x,c){
  if(c==0){return(round(-log(abs(x), base = 10)))}
  else{return(round(-log((abs(x-c)/c) , base = 10)))}
}

test_that("data input error catching", {
  expect_error(data_type_detect(2), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect(c(1,2,3,4,5,6)), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect('hello'), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect(matrix(data = c(1,2,3,45), nrow = 2)), "Please pass a dataframe type structure to the function")
  expect_silent(data_type_detect(basic_test_data))
})
