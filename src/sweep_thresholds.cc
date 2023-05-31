#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

// read in a data frame and output list containing two integer vectors
// lists, containing edge lists for respondent and item graphs
SEXP rsweep_thresholds_agent(SEXP df) 
{
  int ncol = length(df) - 1; // should be column first, right?
  int nrow = length(VECTOR_ELT(df, 0));

  // read a dataframe into a vector of vectors
  std::vector<std::vector<double>> surveytmp(nrow, std::vector<double>(ncol));
  SEXP dummy = PROTECT(allocVector(REALSXP, nrow));
  for(int j = 0; j < ncol; ++j) {
    dummy = VECTOR_ELT(df, j + 1);
    for(int i = 0; i < nrow; ++i) {
      surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  surveygraph S{surveytmp};
  S.sweep_thresholds_agent();

  // put data from threshold_agent (radius, z, lcc) into a list
  SEXP r_agent = PROTECT(allocVector(REALSXP, S.threshold_data_agent.size())); // radius
  SEXP z_agent = PROTECT(allocVector(REALSXP, S.threshold_data_agent.size())); // average degree
  SEXP l_agent = PROTECT(allocVector(REALSXP, S.threshold_data_agent.size())); // LCC

  for(int i = 0; i < S.threshold_data_agent.size(); ++i) {
    REAL(r_agent)[i] = S.threshold_data_agent[i][0];
    REAL(z_agent)[i] = S.threshold_data_agent[i][1];
    REAL(l_agent)[i] = S.threshold_data_agent[i][2];
  }

  SEXP list_agent = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(list_agent, 0, r_agent);
  SET_VECTOR_ELT(list_agent, 1, z_agent);
  SET_VECTOR_ELT(list_agent, 2, l_agent);

  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("threshold"));  // name first column threshold
  SET_STRING_ELT(names, 1, mkChar("ad"));         // name second column ad, for average degree
  SET_STRING_ELT(names, 2, mkChar("lcc"));        // name third column lcc, for largest connected component

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(r_agent);    // number of rows in agent edgelist

  setAttrib(list_agent, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_agent, R_RowNamesSymbol, rownames);
  setAttrib(list_agent, R_NamesSymbol, names);

  UNPROTECT(7);
  return list_agent;
}

SEXP rsweep_thresholds_symbolic(SEXP df) 
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
  S.sweep_thresholds_symbolic();

  // put data from threshold_symbolic (radius, z, lcc) into a list
  SEXP r_symbolic = PROTECT(allocVector(REALSXP, S.threshold_data_symbolic.size())); // radius
  SEXP z_symbolic = PROTECT(allocVector(REALSXP, S.threshold_data_symbolic.size())); // average degree
  SEXP l_symbolic = PROTECT(allocVector(REALSXP, S.threshold_data_symbolic.size())); // LCC

  for(int i = 0; i < S.threshold_data_symbolic.size(); ++i) {
    REAL(r_symbolic)[i] = S.threshold_data_symbolic[i][0];
    REAL(z_symbolic)[i] = S.threshold_data_symbolic[i][1];
    REAL(l_symbolic)[i] = S.threshold_data_symbolic[i][2];
  }

  SEXP list_symbolic = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(list_symbolic, 0, r_symbolic);
  SET_VECTOR_ELT(list_symbolic, 1, z_symbolic);
  SET_VECTOR_ELT(list_symbolic, 2, l_symbolic);

  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("threshold"));  // name first column threshold
  SET_STRING_ELT(names, 1, mkChar("ad"));         // name second column ad, for average degree
  SET_STRING_ELT(names, 2, mkChar("lcc"));        // name third column lcc, for largest connected component

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(r_symbolic);    // number of rows in symbolic edgelist

  setAttrib(list_symbolic, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_symbolic, R_RowNamesSymbol, rownames);
  setAttrib(list_symbolic, R_NamesSymbol, names);

  UNPROTECT(7);
  return list_symbolic;
}
