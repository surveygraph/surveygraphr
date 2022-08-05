#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List ListExamples(const List &rparam)
{
  // accessing all list elements by name
  std::string method = as<std::string>(rparam["method"]);
  double tolerance = as<double>(rparam["tolerance"]);
  int maxIter = as<int>(rparam["maxIter"]);
  Date startDate = Date(as<int>(rparam["startDate"])); // ctor from int

  Rprintf("\nIn C++, seeing the following value\n");
  Rprintf("Method argument    : %s\n", method.c_str());
  Rprintf("Tolerance argument : %f\n", tolerance);
  Rprintf("MaxIter argument   : %d\n", maxIter);
  Rprintf("Start date argument: %04d-%02d-%02d\n", 
          startDate.getYear(), startDate.getMonth(), startDate.getDay());

  return List::create(Named("method", method),
                      Named("tolerance", tolerance),
                      Named("maxIter", maxIter),
                      Named("startDate", startDate),
                      Named("params", rparam));
}
