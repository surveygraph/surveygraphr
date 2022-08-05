#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame RcppRNGs(const int n) 
{
  NumericVector xn = rnorm(n);
  NumericVector xt = rt(n, 1.0);
  NumericVector xp = rpois(n, 1.0);
  
  // create a new data frame to return drawns
  return DataFrame::create(Named("rnorm") = xn, Named("rt") = xt, Named("rpois") = xp);
}
