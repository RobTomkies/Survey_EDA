
test_that("data type forcing working",{
  expect_identical(Data_Describe(basic_test_data,NLP.force = c(1))$data_type_conversion$`converted type`[1],"Natural Language" )
  expect_identical(suppressWarnings(Data_Describe(basic_test_data,ordinal.force = list(c('x', unique(basic_test_data$x))))$data_type_conversion$`converted type`[1]),"Ordinal" )
  expect_identical(Data_Describe(basic_test_data,nominal.force = c(1))$data_type_conversion$`converted type`[1],"Nominal" )
  expect_identical(Data_Describe(basic_test_data,numeric.force = c(1))$data_type_conversion$`converted type`[1],"Integer" )
})

test_that("alternate nas is working",{
  expect_identical(sum(is.na(Data_Describe(basic_test_data,alternate.nas = list(c('x', 22)))$data$x)),as.integer(7) )
})


test_that("preserve non conform functionality is working",{
  expect_identical(ncol(Data_Describe(basic_test_data,preserve.nonconform = F)$data),as.integer(12) )
  expect_identical(ncol(Data_Describe(basic_test_data,preserve.nonconform = T)$data),as.integer(14) )
})

test_that("Check key outputs are correct",{
  x <- unlist(Data_Describe(basic_test_data)$dimensions)
  names(x) <- NULL
  expect_equal(x,as.integer(c(200,200, 12,14)))
  expect_identical(Data_Describe(basic_test_data)$data_type_conversion$`converted type`,c("Integer","Integer","Float","Integer","Natural Language","Nominal","Natural Language","Nominal","Integer","Float","Nominal","Nominal" ))
  expect_identical(as.integer(Data_Describe(basic_test_data)$missingness_percentage),as.integer(c(0,0,0,0,0,5,0,0,12,12,4,0,95,96)))
})

test_that("Input Warnings are correct unique to function",{
  expect_error(Data_Describe(basic_test_data, preserve.nonconform = 'hellnaaah'),'Inputs for "preserve.nonconform" and "remove.repeat" must be logical True or False, please correct')
  expect_error(Data_Describe(basic_test_data, remove.repeat = 'Yeeah'),'Inputs for "preserve.nonconform" and "remove.repeat" must be logical True or False, please correct')
})

test_that("Output data structures are correct types",{
  x <- Data_Describe(basic_test_data)
  expect_equal(is.data.frame(x$data), T)
  expect_equal(is.numeric(x$memory_size), T)
  expect_equal(is.data.frame(x$data_type_conversion), T)
  expect_equal(is.matrix(x$data_type_names), T)
  expect_equal(is.numeric(x$missingness_percentage), T)
  expect_equal(is.data.frame(x$grouped_missingness), T)
  expect_equal(is.numeric(x$repeat_rows), T)
  expect_equal(is.data.frame(x$data), T)
})



