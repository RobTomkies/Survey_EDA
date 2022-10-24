#logrelative error
LRE <- function(x,c){
  if(c==0){return(round(-log(abs(x), base = 10)))}
  else{return(round(-log((abs(x-c)/c) , base = 10)))}
}

test_that("data type forcing error, force type warnings working",{
  expect_warning(column_recog_vector('integer', c(1.1, 2, 3, 4),  basic_test_data), 'Float type number provided for  integer  forcing so converted to integer')
  expect_warning(column_recog_vector('string', c(1.1, 2, 3, 4),  basic_test_data), 'Float type number provided for  string  forcing so converted to integer')
})

test_that("data type forcing, repeat column requests adjusted for",{
  expect_warning(column_recog_vector('integer', c(1,1, 2, 3, 4),  basic_test_data), 'type 1: duplicated found in forcing columns - simplified so only one present')
  expect_warning(column_recog_vector('integer', c('x','x', 'y'),  basic_test_data), 'type 1: duplicated found in forcing columns - simplified so only one present')
})


test_that("data type forcing error, integer index checks", {
  expect_silent(column_recog_vector('integer', c(1, 2, 3, 4),  basic_test_data))
  expect_identical(column_recog_vector('integer', c(1, 2, 3, 4),  basic_test_data), as.integer(c(1,2,3,4)))
  w <- capture_warnings(column_recog_vector('integer', c('x',1),  basic_test_data))
  expect_identical(w[3], "type 2: duplicate columns for forcing found - simplified to only one\n")
})

test_that("data type forcing error, float index checks",{
  expect_warning(column_recog_vector('integer', c(1.1, 2, 3, 4),  basic_test_data), 'Float type number provided for  integer  forcing so converted to integer')
  expect_identical(suppressWarnings(column_recog_vector('integer', c(1.1, 2, 3, 4),  basic_test_data)), as.integer(c(1,2,3,4)))
})

test_that("data type forcing error, logical index checks",{
  expect_silent(column_recog_vector('integer', rep(T, 12),  basic_test_data))
  expect_identical(column_recog_vector('integer', c(T, T, T, F, F,F, T,T,F,F, F, F),  basic_test_data), as.integer(c(1,2,3,7,8)))
  expect_error(column_recog_vector('integer', c(T, T, F, F),  basic_test_data), 'logical forcing vector for  integer  does not match the length of dataset columns please reconsider')
})


test_that("data type forcing error, string index checks",{
  expect_silent(column_recog_vector('integer', c('x', 'y', 'doubls'),  basic_test_data))
  expect_identical(column_recog_vector('integer', c('x', 'y', 'doubls'),  basic_test_data), as.integer(c(1,2,3)))
  w <- capture_warnings(column_recog_vector('integer', c('1.2', '2', '3','4.4', 'words'),  basic_test_data))
  expect_identical(w[2],  "Integer names  2, 3  not found for  integer  forcing but successfully coerced to integer index\n")
  expect_identical(w[3],  "Numeric names  1.2, 4.4  not found for  integer  forcing but successfully coerced to integer index\n")
})

