#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

// read in a data frame and output list containing two integer vectors
// lists, containing edge lists for respondent and item graphs
SEXP surveygraphr_explore_graphs(SEXP df) 
{
  int n = length(df); // should be column first, right?
  int m = length(VECTOR_ELT(df, 0));

  // read a dataframe into a vector of vectors
  std::vector<std::vector<double>> survey(m, std::vector<double> (n));
  SEXP dummy = PROTECT(allocVector(REALSXP, m));
  for(int j = 0; j < n; ++j) {
    dummy = VECTOR_ELT(df, j);
    for(int i = 0; i < m; ++i) {
      //survey[i][j] = (REAL(dummy)[i] - 3) / 2;     // temporary, assumes 1 to 5
      survey[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  // build the corresponding networks
  surveygraph S{survey};
  S.explore_pilot();

  // put data from explore_respondents (radius, z, lcc) into a list
  SEXP e0 = PROTECT(allocVector(REALSXP, S.explore_respondents.size()));
  SEXP e1 = PROTECT(allocVector(REALSXP, S.explore_respondents.size()));
  SEXP e2 = PROTECT(allocVector(REALSXP, S.explore_respondents.size()));

  for(int i = 0; i < S.explore_respondents.size(); ++i) {
    REAL(e0)[i] = S.explore_respondents[i][0];
    REAL(e1)[i] = S.explore_respondents[i][1];
    REAL(e2)[i] = S.explore_respondents[i][2];
  }

  // return a list containing the two edge sets
  SEXP exploredata = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(exploredata, 0, e0);
  SET_VECTOR_ELT(exploredata, 1, e1);
  SET_VECTOR_ELT(exploredata, 2, e2);

  UNPROTECT(5);

  return exploredata;
}
