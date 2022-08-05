#include <Rcpp.h>

#include <cmath>

// suncc needs help to disambiguate between sqrt( float ) and sqrt(double) 
inline static double sqrt_double(double x) { return ::sqrt(x); }

using namespace Rcpp; 

// [[Rcpp::export]]
List MatrixExample(const NumericMatrix &orig) {
  NumericMatrix mat(orig.nrow(), orig.ncol());	

  std::transform(orig.begin(), orig.end(), mat.begin(), sqrt_double);

  return List::create(Named("result") = mat, Named("original") = orig);
}
