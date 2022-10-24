
LRE <- function(x,c){
  if(c==0){return(round(-log(abs(x), base = 10)))}
  else{return(round(-log((abs(x-c)/abs(c)) , base = 10)))}
}

test_that("Input Warnings are correct unique to function",{
  expect_error(NLP_Column_Analysis(basic_test_data$reviews,list(c(1,2,3))), 'column name must be a string')
  expect_error(NLP_Column_Analysis(list(1,2,3,list(2,3,4)),'Reviews'), 'Input data must be a vector of strings, one row for each response')
})

test_that("Output data structures are correct types",{
  x <- suppressWarnings(NLP_Column_Analysis(basic_test_data$reviews, 'Reviews'))
  expect_equal(is.data.frame(x$Word_Frequency), T)
  expect_equal(is.data.frame(x$Bigrams), T)
  expect_equal(is.data.frame(x$Trigrams), T)
  expect_equal(is.data.frame(x$Corr_matrix), T)
  expect_equal(is.data.frame(x$Tf_idf), T)
  expect_equal(is.vector(x$Wordcount), T)
  expect_equal(is.data.frame(x$Sentiments), T)
  expect_equal(is.character(x$input_name), T)
})


test_that("Key Values are correct",{
  set.seed(0)
  x <- suppressWarnings(NLP_Column_Analysis(basic_test_data$reviews, 'Reviews'))

  expect_equal(x$Word_Frequency$n[1], 77)
  expect_identical(x$Word_Frequency$word1[1], 'assignments')

  expect_equal(x$Bigrams$n[1], 19)
  expect_identical(x$Bigrams$word1[1], 'office')
  expect_identical(x$Bigrams$word2[1], 'hours')

  expect_equal(x$Trigrams$n[1], 4)
  expect_identical(x$Trigrams$word1[1], 'future')
  expect_identical(x$Trigrams$word2[1], 'math')
  expect_identical(x$Trigrams$word3[1], 'courses')

  expect_true(LRE(x$Corr_matrix$correlation[1], 0.8039289)>5)
  expect_identical(x$Corr_matrix$item1[1], 'hours')
  expect_identical(x$Corr_matrix$item2[1], 'office')

  expect_equal(x$Tf_idf$response[1], 40)
  expect_true(LRE(x$Tf_idf$tf_idf[1], 5.298317)>5)
  expect_identical(x$Tf_idf$word[1], 'gg')

  expect_length(x$Wordcount, 200)
  expect_equal(x$Wordcount[1], 6)

  expect_equal( x$Sentiments$Reviews_anger[2], 2)
  expect_equal( x$Sentiments$Reviews_trust[7], 7)

  expect_identical(x$input_name, "Reviews")
})
