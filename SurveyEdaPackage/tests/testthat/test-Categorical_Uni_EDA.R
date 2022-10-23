
test_that("Input Warnings are correct unique to function",{
  expect_error(Categorical_Uni_EDA(basic_test_data, detect = 4),'Inputs for "detect" must be logical True or False, please correct')
})

test_that("Output data structures are correct types",{
  x <- Categorical_Uni_EDA(basic_test_data)
  expect_equal(length(names(x)), 6)
  expect_equal(is.data.frame(x$Nominal_Data), T)
  expect_equal(is.null(x$Ordinal_Data), T)
  expect_equal(is.null(x$Ordinal_Statistics), T)
  expect_equal(is.list(x$Nominal_Statistics), T)
  expect_equal(is.list(x$Nominal_Counts), T)
  expect_equal(is.null(x$Ordinal_Counts), T)
  expect_equal(ncol(x$Nominal_Data),4)
  expect_equal(length(x$Nominal_Statistics),4)
  expect_equal(length(x$Nominal_Statistics[[1]]),4)
  y <- suppressWarnings(Categorical_Uni_EDA(basic_test_data, ordinal.force = list(c('x', unique(basic_test_data$x)))))
  expect_equal(is.data.frame(y$Ordinal_Data), T)
  expect_equal(is.list(y$Ordinal_Statistics), T)
})

test_that("Check key outputs are correct",{
  x <- Categorical_Uni_EDA(basic_test_data)
  y <- unlist(x$Nominal_Statistics$nominal_level_uno)
  names(y) <- NULL
  expect_equal(y, c('28','1','15','3'))
  y <- unlist(x$Nominal_Statistics$nominal_letters)
  names(y) <- NULL
  expect_equal(y, c('40','E','21','F'))
  y <- as.numeric(x$Nominal_Counts$nominal_letters)
  names(y) <- NULL
  expect_equal(y, c(29,37,35,38,40,21))
})
