// DataFrame.cpp: data frame example
//
// Copyright (C) 2022 Samuel Unicomb
//
// This file is part of surveygraph.
//
// surveygraph is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// surveygraph is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with surveygraph.  If not, see <http://www.gnu.org/licenses/>.

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
