trial_dataframe <- data.frame(x = c(1,2,3,4,5,6,7,8), y = c(2,3,4,6,7,8,9,5), doubls = c(1.1,2,3,4.1,5.2,4.6,7.1,9.7),actual_int = c('1','2','3','4','5','6','7','8'),words = c('hello', 'my', 'name', 'is', 'rob', 'the', 'great', 'est'))

#logrelative error
LRE <- function(x,c){
  if(c==0){return(round(-log(abs(x), base = 10)))}
  else{return(round(-log((abs(x-c)/c) , base = 10)))}
}

trial_dataframe <- data.frame(x = c(1,2,3,4,5,6,7,8), y = c(2,3,4,6,7,8,9,5), doubls = c(1.1,2,3,4.1,5.2,4.6,7.1,9.7),actual_int = c('1','2','3','4','5','6','7','8'),words = c('hello', 'my', 'name', 'is', 'rob', 'the', 'great', 'est'))
test_that("data input error catching", {
  expect_error(data_type_detect(2), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect(c(1,2,3,4,5,6)), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect('hello'), "Please pass a dataframe type structure to the function")
  expect_error(data_type_detect(matrix(data = c(1,2,3,45), nrow = 2)), "Please pass a dataframe type structure to the function")
  expect_silent(data_type_detect(trial_dataframe))
})

test_that("data type forcing error, force type warnings working",{
  expect_warning(column_recog_vector('integer', c(1.1, 2, 3, 4),  trial_dataframe), 'Float type number provided for  integer  forcing so converted to integer')
  expect_warning(column_recog_vector('string', c(1.1, 2, 3, 4),  trial_dataframe), 'Float type number provided for  string  forcing so converted to integer')
})

test_that("data type forcing, repeat column requests adjusted for",{
  expect_warning(column_recog_vector('integer', c(1,1, 2, 3, 4),  trial_dataframe), 'type 1: duplicated found in forcing columns - simplified so only one present')
  expect_warning(column_recog_vector('integer', c('x','x', 'y'),  trial_dataframe), 'type 1: duplicated found in forcing columns - simplified so only one present')
})


test_that("data type forcing error, integer index checks", {
  expect_silent(column_recog_vector('integer', c(1, 2, 3, 4),  trial_dataframe))
  expect_identical(column_recog_vector('integer', c(1, 2, 3, 4),  trial_dataframe), as.integer(c(1,2,3,4)))
  w <- capture_warnings(column_recog_vector('integer', c('x',1),  trial_dataframe))
  expect_identical(w[3], "type 2: duplicate columns for forcing found - simplified to only one")
})

test_that("data type forcing error, float index checks",{
  expect_warning(column_recog_vector('integer', c(1.1, 2, 3, 4),  trial_dataframe), 'Float type number provided for  integer  forcing so converted to integer')
  expect_identical(suppressWarnings(column_recog_vector('integer', c(1.1, 2, 3, 4),  trial_dataframe)), as.integer(c(1,2,3,4)))
})

test_that("data type forcing error, logical index checks",{
  expect_silent(column_recog_vector('integer', c(T, T, T, F, F),  trial_dataframe))
  expect_identical(column_recog_vector('integer', c(T, T, T, F, F),  trial_dataframe), as.integer(c(1,2,3)))
  expect_error(column_recog_vector('integer', c(T, T, F, F),  trial_dataframe), 'logical forcing vector for  integer  does not match the length of dataset columns please reconsider')
})

test_that("data type forcing error, string index checks",{
  #needs to be completed
})
