#include <Rcpp.h>

using namespace Rcpp;

template <class T> T mytolower (T c)
{
  static std::locale loc;
  return std::tolower(c, loc);
}

// [[Rcpp::export]]
List StringVectorExample(const StringVector &orig)
{
  StringVector vec(orig.size());	

  std::transform(orig.begin(), orig.end(), vec.begin(), 
     make_string_transformer(mytolower<char>) );

  return List::create(Named("result") = vec, Named("original") = orig);
}
