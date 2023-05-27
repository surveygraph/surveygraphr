#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

// read in a data frame and output list containing two integer vectors
// lists, containing edge lists for respondent and item graphs
SEXP surveygraphr_sweep_thresholds(SEXP df) 
{
  int ncol = length(df) - 1; // should be column first, right?
  int nrow = length(VECTOR_ELT(df, 0));

  // read a dataframe into a vector of vectors
  std::vector<std::vector<double>> surveytmp(nrow, std::vector<double>(ncol));
  SEXP dummy = PROTECT(allocVector(REALSXP, nrow));
  for(int j = 0; j < ncol; ++j) {
    dummy = VECTOR_ELT(df, j + 1);
    for(int i = 0; i < nrow; ++i) {
      //surveytmp[i][j] = (REAL(dummy)[i] - 3) / 2;     // temporary, assumes 1 to 5
      surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  surveygraph S{surveytmp};
  S.sweep_thresholds();

  // put data from threshold_respondents (radius, z, lcc) into a list
  SEXP r_respondents = PROTECT(allocVector(REALSXP, S.threshold_respondents.size())); // radius
  SEXP z_respondents = PROTECT(allocVector(REALSXP, S.threshold_respondents.size())); // average degree
  SEXP l_respondents = PROTECT(allocVector(REALSXP, S.threshold_respondents.size())); // LCC

  for(int i = 0; i < S.threshold_respondents.size(); ++i) {
    REAL(r_respondents)[i] = S.threshold_respondents[i][0];
    REAL(z_respondents)[i] = S.threshold_respondents[i][1];
    REAL(l_respondents)[i] = S.threshold_respondents[i][2];
  }

  SEXP list_respondents = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(list_respondents, 0, r_respondents);
  SET_VECTOR_ELT(list_respondents, 1, z_respondents);
  SET_VECTOR_ELT(list_respondents, 2, l_respondents);

  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("threshold"));  // name first column x
  SET_STRING_ELT(names, 1, mkChar("avgdegree"));  // name second column y
  SET_STRING_ELT(names, 2, mkChar("lcc"));        // name second column y

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(r_respondents);    // number of rows in respondents edgelist

  setAttrib(list_respondents, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_respondents, R_RowNamesSymbol, rownames);
  setAttrib(list_respondents, R_NamesSymbol, names);

  // put data from item_respondents (radius, z, lcc) into a list
  SEXP r_items = PROTECT(allocVector(REALSXP, S.threshold_items.size())); // radius
  SEXP z_items = PROTECT(allocVector(REALSXP, S.threshold_items.size())); // average degree
  SEXP l_items = PROTECT(allocVector(REALSXP, S.threshold_items.size())); // LCC

  for(int i = 0; i < S.threshold_items.size(); ++i) {
    REAL(r_items)[i] = S.threshold_items[i][0];
    REAL(z_items)[i] = S.threshold_items[i][1];
    REAL(l_items)[i] = S.threshold_items[i][2];
  }

  SEXP list_items = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(list_items, 0, r_items);
  SET_VECTOR_ELT(list_items, 1, z_items);
  SET_VECTOR_ELT(list_items, 2, l_items);

  INTEGER(rownames)[1] = -length(r_items);    // number of rows in respondents edgelist

  setAttrib(list_items, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_items, R_RowNamesSymbol, rownames);
  setAttrib(list_items, R_NamesSymbol, names);

  // return a list containing the two edge sets
  SEXP thresholdlist = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(thresholdlist, 0, list_respondents);
  SET_VECTOR_ELT(thresholdlist, 1, list_items);

  UNPROTECT(12);
  return thresholdlist;
}
