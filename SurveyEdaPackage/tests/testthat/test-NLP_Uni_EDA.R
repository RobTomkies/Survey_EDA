test_that("correct warnings and errors",{
  w <- capture_warnings(NLP_Uni_EDA(basic_test_data))
  expect_identical(w[1], 'fewer than 5 meaningful words in column, word corrolation analysis not possible')
})

test_that("Output data structures are correct types",{
  x <- suppressWarnings(NLP_Uni_EDA(basic_test_data))
  expect_equal(length(x) , 4)
  expect_equal(length(x[1]), length(x[2]))
})
