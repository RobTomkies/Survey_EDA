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
  expect_error(data_type_detect(basic_test_data, preserve_nonconform = 42),'Inputs for "preserve_nonconform"  must be logical True or False, please correct')
  expect_silent(data_type_detect(basic_test_data))
})


test_that("Check key outputs are correct",{
  x <- data_type_detect(basic_test_data)
  expect_identical(x$converted_type$data_type,c("Integer","Integer","Float","Integer","Natural Language","Nominal","Natural Language","Nominal","Integer","Float","Nominal","Nominal" ))
  expect_identical(x$original_type$data_type,c("Integer","Integer","Float","Integer","Natural Language","Integer","Natural Language", "Natural Language", "Integer","Float","Natural Language", "Natural Language" ))
})

test_that("data type forcing working",{
  expect_identical(data_type_detect(basic_test_data,NLP_force = c(1))$converted_type$data_type[1],"Natural Language" )
  expect_identical(suppressWarnings(data_type_detect(basic_test_data,ordinal_force = list(c('x', unique(basic_test_data$x))))$converted_type$data_type[1]),"Ordinal" )
  expect_identical(data_type_detect(basic_test_data,nominal_force = c(1))$converted_type$data_type[1],"Nominal" )
  expect_identical(data_type_detect(basic_test_data,numeric_force = c(1))$converted_type$data_type[1],"Integer" )
})

test_that("Output data structures are correct types",{
  x <- data_type_detect(basic_test_data)
  expect_equal(is.data.frame(x$data), T)
  expect_equal(is.data.frame(x$original_type), T)
  expect_equal(is.data.frame(x$converted_type), T)
})
