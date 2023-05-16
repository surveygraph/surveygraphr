#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

static void vectors_to_df(map<int, set<neighbour>> &g, SEXP &df)
{
  int dummysize = 0;
  for(auto &it : g){
    for(auto &jt : it.second){
      if(it.first < jt.u) dummysize += 1;
    }
  }
  SEXP u_vector = PROTECT(allocVector(INTSXP, dummysize));  // u column
  SEXP v_vector = PROTECT(allocVector(INTSXP, dummysize));  // v column
  SEXP w_vector = PROTECT(allocVector(REALSXP, dummysize)); // weight column

  int i = 0;
  for(auto &it : g){
    for(auto &jt : it.second){
      if(it.first < jt.u){
        INTEGER(u_vector)[i] = it.first + 1;
        INTEGER(v_vector)[i] = jt.u + 1;
        REAL(w_vector)[i] = int(100.0 * jt.w) / 100.0;
        i += 1;
      }
    }
  }

  SET_VECTOR_ELT(df, 0, u_vector);
  SET_VECTOR_ELT(df, 1, v_vector);
  SET_VECTOR_ELT(df, 2, w_vector);

  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("u"));            // name first column u
  SET_STRING_ELT(names, 1, mkChar("v"));            // name second column v
  SET_STRING_ELT(names, 2, mkChar("weight"));       // name third column weight, required by igraph

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(u_vector);         // number of rows in respondents edgelist

  setAttrib(df, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(df, R_RowNamesSymbol, rownames);
  setAttrib(df, R_NamesSymbol, names);

  UNPROTECT(5);
}

// read in a data frame and output list containing two integer vectors
// lists, containing edge lists for respondent and item graphs
SEXP surveygraphr_graph_edgelists(SEXP df, SEXP rlcc, SEXP ilcc) 
{
  int n = length(df) - 1; // should be column first, right?
  int m = length(VECTOR_ELT(df, 0));

  std::vector<std::vector<double>> surveytmp(m, std::vector<double>(n));
  SEXP dummy = PROTECT(allocVector(REALSXP, m));
  for(int j = 0; j < n; ++j){
    dummy = VECTOR_ELT(df, j + 1);
    for(int i = 0; i < m; ++i){
      surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  surveygraph S{surveytmp, REAL(rlcc)[0], REAL(ilcc)[0]};

  S.graph_edgelists_pilot();

  SEXP list_respondents = PROTECT(allocVector(VECSXP, 3));
  SEXP list_items = PROTECT(allocVector(VECSXP, 3));

  vectors_to_df(S.g_respondents, list_respondents);
  vectors_to_df(S.g_items, list_items);

  SEXP edgelists = PROTECT(allocVector(VECSXP, 2)); // list of two dataframes
  SET_VECTOR_ELT(edgelists, 0, list_respondents);
  SET_VECTOR_ELT(edgelists, 1, list_items);

  UNPROTECT(4);

  return edgelists;
}
