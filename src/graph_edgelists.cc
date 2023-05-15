#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

// read in a data frame and output list containing two integer vectors
// lists, containing edge lists for respondent and item graphs
SEXP surveygraphr_graph_edgelists(SEXP df, SEXP rlcc, SEXP ilcc) 
{
  int n = length(df); // should be column first, right?
  int m = length(VECTOR_ELT(df, 0));

  // read a dataframe into a vector of vectors
  std::vector<std::vector<double>> surveytmp(m, std::vector<double> (n));
  SEXP dummy = PROTECT(allocVector(REALSXP, m));
  for(int j = 0; j < n; ++j) {
    dummy = VECTOR_ELT(df, j);
    for(int i = 0; i < m; ++i) {
      //surveytmp[i][j] = (REAL(dummy)[i] - 3) / 2;     // temporary, assumes 1 to 5
      surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  double rlcctmp = REAL(rlcc)[0];
  double ilcctmp = REAL(ilcc)[0];
  surveygraph S{surveytmp, rlcctmp, ilcctmp}; // constructor for surveygraph class
  S.graph_edgelists_pilot();

  // count edges in respondent graph
  int ecount = 0;
  for(auto &it : S.g_respondents) ecount += it.second.size();
  if(ecount % 2 == 0) {
    ecount /= 2;
  } else {
    Rprintf("ERROR: respondent graph has uneven number of edges\n");
  }
  SEXP u_respondents = PROTECT(allocVector(INTSXP, ecount));  // u column, respondents
  SEXP v_respondents = PROTECT(allocVector(INTSXP, ecount));  // v column, respondents
  SEXP w_respondents = PROTECT(allocVector(REALSXP, ecount)); // weight column, respondents

  // count edges in item graph
  ecount = 0;
  for(auto &it : S.g_items) ecount += it.second.size();
  if(ecount % 2 == 0) {
    ecount /= 2;
  } else {
    Rprintf("ERROR: item graph has uneven number of edges\n");
  }
  SEXP u_items = PROTECT(allocVector(INTSXP, ecount));  // u column, items
  SEXP v_items = PROTECT(allocVector(INTSXP, ecount));  // v column, items
  SEXP w_items = PROTECT(allocVector(REALSXP, ecount)); // weight column, items

  // create integer vector for respondent graph edge list
  int i = 0;
  for(auto &it : S.g_respondents) {
    for(auto &jt : it.second) {
      if(it.first < jt.u) {
        INTEGER(u_respondents)[i] = it.first + 1;
        INTEGER(v_respondents)[i] = jt.u + 1;
        REAL(w_respondents)[i] = jt.w;
        i += 1;
      }
    }
  }

  // create integer vector for item graph edge list
  i = 0;
  for(auto &it : S.g_items) {
    for(auto &jt : it.second) {
      if(it.first < jt.u) {
        INTEGER(u_items)[i] = it.first + 1;
        INTEGER(v_items)[i] = jt.u + 1;
        REAL(w_items)[i] = jt.w;
        i += 1;
      }
    }
  }
  // make dataframe of respondent edgelist
  SEXP list_respondents = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(list_respondents, 0, u_respondents);
  SET_VECTOR_ELT(list_respondents, 1, v_respondents);
  SET_VECTOR_ELT(list_respondents, 2, w_respondents);

  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("u"));            // name first column x
  SET_STRING_ELT(names, 1, mkChar("v"));            // name second column y
  SET_STRING_ELT(names, 2, mkChar("w"));            // name second column y

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(u_respondents);    // number of rows in respondents edgelist

  setAttrib(list_respondents, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_respondents, R_RowNamesSymbol, rownames);
  setAttrib(list_respondents, R_NamesSymbol, names);

  // make dataframe of item edgelist
  SEXP list_items = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(list_items, 0, u_items);
  SET_VECTOR_ELT(list_items, 1, v_items);
  SET_VECTOR_ELT(list_items, 2, w_items);

  INTEGER(rownames)[1] = -length(u_items);    // number of rows in item edgelist

  setAttrib(list_items, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_items, R_RowNamesSymbol, rownames);
  setAttrib(list_items, R_NamesSymbol, names);

  // return a list containing the two edge sets
  SEXP edgelists = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(edgelists, 0, list_respondents);
  SET_VECTOR_ELT(edgelists, 1, list_items);

  //UNPROTECT(3);
  UNPROTECT(12);

  //return e_respondents;
  return edgelists;
}
