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
SEXP surveygraphr_make_projection(SEXP df, SEXP rlcc, SEXP ilcc) 
{
  int ncol = length(df) - 1; // should be column first, right?
  int nrow = length(VECTOR_ELT(df, 0));

  std::vector<std::vector<double>> surveytmp(nrow, std::vector<double>(ncol));
  SEXP dummy = PROTECT(allocVector(REALSXP, nrow));
  for(int j = 0; j < ncol; ++j){
    dummy = VECTOR_ELT(df, j + 1);
    for(int i = 0; i < nrow; ++i){
      surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  surveygraph S{surveytmp, REAL(rlcc)[0], REAL(ilcc)[0]};

  S.make_projection();

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

SEXP surveygraphr_make_projection_agent(SEXP df, SEXP rlcc) 
{
  int ncol = length(df) - 1; // should be column first, right?
  int nrow = length(VECTOR_ELT(df, 0));

  std::vector<std::vector<double>> surveytmp(nrow, std::vector<double>(ncol));
  SEXP dummy = PROTECT(allocVector(REALSXP, nrow));
  for(int j = 0; j < ncol; ++j){
    dummy = VECTOR_ELT(df, j + 1);
    for(int i = 0; i < nrow; ++i){
      surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  surveygraph S{surveytmp, REAL(rlcc)[0], 0.95};

  S.make_projection_agent();

  SEXP list_respondents = PROTECT(allocVector(VECSXP, 3));

  vectors_to_df(S.g_respondents, list_respondents);

  UNPROTECT(2);

  return list_respondents;
}

SEXP surveygraphr_make_projection_symbolic(SEXP df, SEXP ilcc) 
{
  int ncol = length(df) - 1; // should be column first, right?
  int nrow = length(VECTOR_ELT(df, 0));

  std::vector<std::vector<double>> surveytmp(nrow, std::vector<double>(ncol));
  SEXP dummy = PROTECT(allocVector(REALSXP, nrow));
  for(int j = 0; j < ncol; ++j){
    dummy = VECTOR_ELT(df, j + 1);
    for(int i = 0; i < nrow; ++i){
      surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  surveygraph S{surveytmp, 0.95, REAL(ilcc)[0]};

  S.make_projection_symbolic();

  SEXP list_symbolic = PROTECT(allocVector(VECSXP, 3));

  vectors_to_df(S.g_items, list_symbolic);

  UNPROTECT(2);

  return list_symbolic;
}
