#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

// [[Rcpp::export]]
NumericVector f2(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for(int i = 1; i < n; ++i) {
    out[i] = out[i -1] + x[i];
  }
  return out;
}

// [[Rcpp::export]]
NumericVector rowSumsC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j <ncol; j++) {
      total += x(i, j);
    }
  out[i] = total;  
  }
  return out;
}

// [[Rcpp::export]]
double processDF(DataFrame df){
    CharacterVector x = df["x"] ;
    NumericVector   y = df["y"] ;
    IntegerVector   z = df["v"] ;

    /* do whatever with x, y, v */
    double res = sum(y) ;
    return res ;
}

/*** R
library(microbenchmark)
x <- runif(1e5)
microbenchmark(
  mean(x),
  meanC(x))

x <- runif(20)
x
f2(x)

set.seed(1014)
x <- matrix(sample(100), 10)
microbenchmark(
  rowSums(x),
  rowSumsC(x))
*/
