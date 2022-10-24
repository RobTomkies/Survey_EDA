#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector RCPPColMean(NumericMatrix x) {
  NumericMatrix y = clone(x);
  int ncol = y.ncol();
  NumericVector out(ncol);
  for (int i = 0; i < ncol; i++) {
    NumericVector col = y( _ , i );
    out[i] = mean(na_omit(col));

  }
  return out;
}


// [[Rcpp::export]]
NumericVector RCPPColMedian(NumericMatrix x) {
  NumericMatrix y = clone(x);
  int ncol = y.ncol();
  NumericVector out(ncol);
  for (int i = 0; i < ncol; i++) {
    NumericVector col = y( _ , i );
    out[i] = median(na_omit(col));

  }
  return out;
}

// [[Rcpp::export]]
NumericVector RCPPColSD(NumericMatrix x) {
  NumericMatrix y = clone(x);
  int ncol = y.ncol();
  NumericVector out(ncol);
  for (int i = 0; i < ncol; i++) {
    NumericVector col = y( _ , i );
    out[i] = sd(na_omit(col));

  }
  return out;
}


// [[Rcpp::export]]
NumericVector RCPPColMax(NumericMatrix x) {
  NumericMatrix y = clone(x);
  int ncol = y.ncol();
  NumericVector out(ncol);
  for (int i = 0; i < ncol; i++) {
    NumericVector col = y( _ , i );
    out[i] = max(na_omit(col));

  }
  return out;
}

// [[Rcpp::export]]
NumericVector RCPPColMin(NumericMatrix x) {
  NumericMatrix y = clone(x);
  int ncol = y.ncol();
  NumericVector out(ncol);
  for (int i = 0; i < ncol; i++) {
    NumericVector col = y( _ , i );

    out[i] = min(na_omit(col));

  }
  return out;
}

// [[Rcpp::export]]
NumericVector RCPPColSkew(NumericMatrix x) {
  NumericMatrix y = clone(x);
  int ncol = y.ncol();
  NumericVector out(ncol);
  for (int i = 0; i < ncol; i++) {
    NumericVector col = y( _ , i );
    double colmean = mean(na_omit(col));
    double colmedian = median(na_omit(col));
    double colSD = sd(na_omit(col));
    double skew = (3*(colmean - colmedian))/colSD;
    out[i] = skew;
  }
  return out;
}

// Test R code: automatically
// run after the compilation.
//

/*** R
x<- c(1,2,3,4,5,6,7,8)
y<- c(3,4,5,6,7,NA,5,34)
z<- as.matrix(data.frame(x,y))
RCPPColMean(z)
RCPPColMedian(z)
RCPPColSD(z)
RCPPColMax(z)
RCPPColMin(z)
RCPPColSkew(z)
*/





