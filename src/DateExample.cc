#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List DateExample(DateVector &dv, DatetimeVector &dtv)
{
  // Support for this changed with Rcpp 0.12.8 but is still optional
  // Support for << redirection added added with 0.12.8.2 and later
  Rcout << "\nIn C++, seeing the following date values before/after adding a week:\n"
        << dv << std::endl;
  dv = dv + 7;		// shift a week
  Rcout << dv << std::endl;

  Rcout << "\nIn C++, seeing the following datetime values before/after adding a quarter second:\n"
        << dtv << std::endl;
  dtv = dtv + 0.250;          // shift 250 millisec
  Rcout << dtv << std::endl;

  // Build result set to be returned as a list to R.
  return List::create(Named("date", dv), Named("datetime", dtv));
}
