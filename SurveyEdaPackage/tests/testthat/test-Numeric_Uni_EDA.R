#logrelative error
LRE <- function(x,c){
  if(c==0){return(round(-log(abs(x), base = 10)))}
  else{return(round(-log((abs(x-c)/abs(c)) , base = 10)))}
}

test_that("Input Warnings are correct unique to function",{
  expect_error(Numeric_Uni_EDA(basic_test_data, ignore.columns = c(names(basic_test_data))),'No numeric data either detected or forced, please correct')
})

test_that("Output data structures are correct types",{
  x <- Numeric_Uni_EDA(basic_test_data)
  expect_equal(length(names(x)), 5)
  expect_equal(is.list(x$Integer_Characteristics), T)
  expect_equal(is.list(x$Float_Characteristics), T)
  expect_equal(is.list(x$GT_20pcnt_Flag), T)
  expect_equal(is.data.frame(x$Interger_Data), T)
  expect_equal(is.data.frame(x$Double_Data), T)
})


test_that("Check Integer key outputs are correct",{
  x <- Numeric_Uni_EDA(basic_test_data)
  expect_true(LRE(x$Integer_Characteristics$Means[1], 48.7400) > 5)
  expect_true(LRE(x$Integer_Characteristics$Means[2], 107.4450) > 5)
  expect_true(LRE(x$Integer_Characteristics$Means[3], 398.1800) > 5)
  expect_true(LRE(x$Integer_Characteristics$Means[4], 395.6023) > 5)

  expect_true(LRE(x$Integer_Characteristics$Medians[1], 45.0 ) > 5)
  expect_true(LRE(x$Integer_Characteristics$Medians[2], 112.0) > 5)
  expect_true(LRE(x$Integer_Characteristics$Medians[3], 422.5) > 5)
  expect_true(LRE(x$Integer_Characteristics$Medians[4], 413.5 ) > 5)

  expect_true(LRE(x$Integer_Characteristics$Maxs[1], 100 ) > 5)
  expect_true(LRE(x$Integer_Characteristics$Maxs[2], 200) > 5)
  expect_true(LRE(x$Integer_Characteristics$Maxs[3], 800) > 5)
  expect_true(LRE(x$Integer_Characteristics$Maxs[4], 800 ) > 5)

  expect_true(LRE(x$Integer_Characteristics$Mins[1], 2 ) > 5)
  expect_true(LRE(x$Integer_Characteristics$Mins[2], 1) > 5)
  expect_true(LRE(x$Integer_Characteristics$Mins[3], 12) > 5)
  expect_true(LRE(x$Integer_Characteristics$Mins[4], 14 ) > 5)

  expect_true(LRE(x$Integer_Characteristics$I.Q.Rs[1], 45.0 ) > 5)
  expect_true(LRE(x$Integer_Characteristics$I.Q.Rs[2], 117.50) > 5)
  expect_true(LRE(x$Integer_Characteristics$I.Q.Rs[3], 396.25) > 5)
  expect_true(LRE(x$Integer_Characteristics$I.Q.Rs[4], 394.75 ) > 5)

  expect_true(LRE(x$Integer_Characteristics$S.Ds[1], 26.55595 ) > 5)
  expect_true(LRE(x$Integer_Characteristics$S.Ds[2], 62.89456) > 5)
  expect_true(LRE(x$Integer_Characteristics$S.Ds[3], 230.32544) > 5)
  expect_true(LRE(x$Integer_Characteristics$S.Ds[4], 229.73637 ) > 5)

  expect_true(LRE(x$Integer_Characteristics$Skews[1], 0.4225042 ) > 5)
  expect_true(LRE(x$Integer_Characteristics$Skews[2], -0.2172684) > 5)
  expect_true(LRE(x$Integer_Characteristics$Skews[3], -0.3167692) > 5)
  expect_true(LRE(x$Integer_Characteristics$Skews[4], -0.2337165) > 5)

  expect_identical(x$Integer_Characteristics$Modes$x, c(22.))
  expect_identical(x$Integer_Characteristics$Modes$y, c(195.))
  expect_identical(x$Integer_Characteristics$Modes$actual_int, c(457., 672, 800))
  expect_identical(x$Integer_Characteristics$Modes$ints_with_na, c(672,800.))
})

test_that("Check float type key outputs are correct",{
  x <- Numeric_Uni_EDA(basic_test_data)
  expect_true(LRE(x$Float_Characteristics$Means[1], 48.11823) > 5)
  expect_true(LRE(x$Float_Characteristics$Means[2], 48.51815) > 5)

  expect_true(LRE(x$Float_Characteristics$Medians[1], 46.15422  ) > 5)
  expect_true(LRE(x$Float_Characteristics$Medians[2], 46.26620 ) > 5)

  expect_true(LRE(x$Float_Characteristics$Maxs[1], 98.49511 ) > 5)
  expect_true(LRE(x$Float_Characteristics$Maxs[2], 98.49511) > 5)

  expect_true(LRE(x$Float_Characteristics$Mins[1], 0.1947149  ) > 5)
  expect_true(LRE(x$Float_Characteristics$Mins[2], 0.1947149 ) > 5)

  expect_true(LRE(x$Float_Characteristics$I.Q.Rs[1], 48.63634  ) > 5)
  expect_true(LRE(x$Float_Characteristics$I.Q.Rs[2], 50.45301 ) > 5)

  expect_true(LRE(x$Float_Characteristics$S.Ds[1],  28.07992 ) > 5)
  expect_true(LRE(x$Float_Characteristics$S.Ds[2], 28.18401) > 5)

  expect_true(LRE(x$Float_Characteristics$Skews[1], 0.2098306) > 5)
  expect_true(LRE(x$Float_Characteristics$Skews[2], 0.2397047 ) > 5)

  expect_equal(x$Float_Characteristics$Modes['doubles_with_na'], c(doubles_with_na = "More than 10 values") )
  expect_equal(x$Float_Characteristics$Modes['doubls'], c(doubls = "More than 10 values" ))
})


