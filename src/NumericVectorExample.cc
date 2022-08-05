#include <Rcpp.h>

#include <cmath>

using namespace Rcpp; 

// suncc needs help to disambiguate between sqrt(float) and sqrt(double) 
inline static double sqrt_double(double x){ return ::sqrt(x); }

// [[Rcpp::export]]
List NumericVectorExample(const NumericVector &orig)
{
  NumericVector vec(orig.size()); // create a target vector of the same size
  
  std::transform(orig.begin(), orig.end(), vec.begin(), sqrt_double);

  return List::create(Named("result") = vec, Named("original") = orig);
}
