#' Basic Survey Test Data (basic_test_data)
#'
#' An example dataset designed to illustrate the functionality of the package. Data is either randomly generated or source from the University of Waterloo course reviews dataset on kaggle
#'
#'
#' @format ## `basic_test_data`
#' A data frame with 200 rows and 12 columns:
#' \describe{
#'   \item{x}{Randomly generated integer data generated from uniform random sampling the integers 1-100, 200 times}
#'   \item{y}{Randomly generated integer data generated from uniform random sampling the integers 1-200, 200 times}
#'   \item{doubls}{Float type data generated from generatign random numbers between 0 and 100, 200 times}
#'   \item{actual_int}{Randomly generated integer data generated from uniform random sampling the integers 1-800, 200 times}
#'   \item{words}{200 randomly generated words using the randomWords function}
#'   \item{nominal_level_uno}{Randomly generated integer data generated from uniform random sampling the integers 1-800, 200 times}
#'   \item{reviews}{200 top reviews from the University of Waterloo reviews dataset}
#'   \item{nominal_letters}{200 randomly generated letters from A-F}
#'   \item{ints_with_na}{actual_int however with NA values randomly injected}
#'   \item{doubles_with_na}{doubls data however with NA values randomly injected}
#'   \item{nominal_with_written}{nominal letters however with free text randomly injected}
#'   \item{nominal_with_writtenb}{nominal letters however with free text randomly injected}
#'   ...
#' }
#' @source <https://www.kaggle.com/datasets/anthonysusevski/course-reviews-university-of-waterloo>
"basic_test_data"


#' University of Waterloo Course Reviews (natural_language_column)
#'
#' Top 1000 reviews from University of Waterloo course reviews dataset on kaggle
#'
#'
#' @format ## `natural_language_column`
#' A data frame with 200 rows and 12 columns:
#' \describe{
#'   \item{reviews}{Top 1000 reviews from the Waterloo course reviews dataset}
#'   ...
#' }
#' @source <https://www.kaggle.com/datasets/anthonysusevski/course-reviews-university-of-waterloo>
"natural_language_column"
