#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

static void df_to_cppvector(const SEXP &df, std::vector<vector<double>> &stmp)
{
  // check column types
  //SEXP check = PROTECT(allocVector(VECSXP, length(df)));
  //Rprintf("df is of type: %d\n", TYPEOF(df));
  //for(int i = 0; i < length(df); ++i){
  //  check = VECTOR_ELT(df, i);
  //  Rprintf("%d has type: %d\n", TYPEOF(check));
  //}

  vector<vector<double>> surveytmp;

  // idea: use numeric coercion... if not coercable (returns NULL?) skip column
  SEXP check = PROTECT(allocVector(VECSXP, length(df)));
  for(int i = 0; i < length(df); ++i){
    check = VECTOR_ELT(df, i);
    if(TYPEOF(check) == STRSXP){
      //Rprintf("you've found a string column\n");
    }else if(TYPEOF(check) == REALSXP){
      vector<double> coltmp;
      for(int j = 0; j < length(check); ++j){
        coltmp.push_back(((REAL(check)[j]) - 5.5) / 4.5);
      }
      surveytmp.push_back(coltmp);
    }else if(TYPEOF(check) == INTSXP){
      vector<double> coltmp;
      for(int j = 0; j < length(check); ++j){
        coltmp.push_back((double(INTEGER(check)[j]) - 5.5) / 4.5);
      }
      surveytmp.push_back(coltmp);
    }else{
      //Rprintf("you've found a column that's neither string, real or int\n");
    }
  }

  int ncol = surveytmp.size();
  int nrow = surveytmp[0].size();

  // take the transpose
  stmp = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));
  for(int i = 0; i < surveytmp.size(); ++i){
    for(int j = 0; j < surveytmp[i].size(); ++j){
      stmp[j][i] = surveytmp[i][j];
    }
  }

  UNPROTECT(1);
}

// read in a data frame and output list containing two integer vectors
// lists, containing edge lists for respondent and item graphs
SEXP rmake_threshold_profile_agent(SEXP df) 
{
  //int ncol = length(df); // should be column first, right?
  //int nrow = length(VECTOR_ELT(df, 0));

  //// read a dataframe into a vector of vectors
  //std::vector<std::vector<double>> surveytmp(nrow, std::vector<double>(ncol));
  //SEXP dummy = PROTECT(allocVector(REALSXP, nrow));
  //for(int j = 0; j < ncol; ++j) {
  //  dummy = VECTOR_ELT(df, j);
  //  for(int i = 0; i < nrow; ++i) {
  //    surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
  //  }
  //}

  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);

  surveygraph S{surveytmp};
  S.make_threshold_profile_agent();

  // put data from threshold_agent (threshold, z, lcc) into a list
  SEXP t_agent = PROTECT(allocVector(REALSXP, S.threshold_data_agent.size())); // threshold
  SEXP z_agent = PROTECT(allocVector(REALSXP, S.threshold_data_agent.size())); // average degree
  SEXP l_agent = PROTECT(allocVector(REALSXP, S.threshold_data_agent.size())); // LCC
  SEXP i_agent = PROTECT(allocVector(INTSXP, S.threshold_data_agent.size())); // isolated node count
  SEXP c_agent = PROTECT(allocVector(INTSXP, S.threshold_data_agent.size())); // component count

  for(int i = 0; i < S.threshold_data_agent.size(); ++i) {
    REAL(t_agent)[i] = S.threshold_data_agent[i][0];
    REAL(z_agent)[i] = S.threshold_data_agent[i][1];
    REAL(l_agent)[i] = S.threshold_data_agent[i][2];
    INTEGER(i_agent)[i] = int(S.threshold_data_agent[i][3] + 0.5);
    INTEGER(c_agent)[i] = int(S.threshold_data_agent[i][4] + 0.5);
  }

  SEXP list_agent = PROTECT(allocVector(VECSXP, 5));
  SET_VECTOR_ELT(list_agent, 0, t_agent);
  SET_VECTOR_ELT(list_agent, 1, z_agent);
  SET_VECTOR_ELT(list_agent, 2, l_agent);
  SET_VECTOR_ELT(list_agent, 3, i_agent);
  SET_VECTOR_ELT(list_agent, 4, c_agent);

  SEXP names = PROTECT(allocVector(STRSXP, 5));
  SET_STRING_ELT(names, 0, mkChar("threshold"));   // name first column threshold
  SET_STRING_ELT(names, 1, mkChar("ad"));          // name second column ad, for average degree
  SET_STRING_ELT(names, 2, mkChar("lcc"));         // name third column lcc, for largest connected component
  SET_STRING_ELT(names, 3, mkChar("isolated"));    // name fourth column isolated, for number of isolated nodes
  SET_STRING_ELT(names, 4, mkChar("components"));  // name fifth column components, for number of components

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(t_agent);    // number of rows in agent edgelist

  setAttrib(list_agent, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_agent, R_RowNamesSymbol, rownames);
  setAttrib(list_agent, R_NamesSymbol, names);

  UNPROTECT(8);
  return list_agent;
}

SEXP rmake_threshold_profile_symbolic(SEXP df) 
{
  //int ncol = length(df); // should be column first, right?
  //int nrow = length(VECTOR_ELT(df, 0));

  //// read a dataframe into a vector of vectors
  //std::vector<std::vector<double>> surveytmp(nrow, std::vector<double>(ncol));
  //SEXP dummy = PROTECT(allocVector(REALSXP, nrow));
  //for(int j = 0; j < ncol; ++j) {
  //  dummy = VECTOR_ELT(df, j);
  //  for(int i = 0; i < nrow; ++i) {
  //    //surveytmp[i][j] = (REAL(dummy)[i] - 3) / 2;     // temporary, assumes 1 to 5
  //    surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
  //  }
  //}

  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);

  surveygraph S{surveytmp};
  S.make_threshold_profile_symbolic();

  // put data from threshold_symbolic (threshold, z, lcc) into a list
  SEXP t_symbolic = PROTECT(allocVector(REALSXP, S.threshold_data_symbolic.size())); // threshold
  SEXP z_symbolic = PROTECT(allocVector(REALSXP, S.threshold_data_symbolic.size())); // average degree
  SEXP l_symbolic = PROTECT(allocVector(REALSXP, S.threshold_data_symbolic.size())); // LCC
  SEXP i_symbolic = PROTECT(allocVector(INTSXP, S.threshold_data_symbolic.size())); // isolated node count
  SEXP c_symbolic = PROTECT(allocVector(INTSXP, S.threshold_data_symbolic.size())); // number of components

  for(int i = 0; i < S.threshold_data_symbolic.size(); ++i) {
    REAL(t_symbolic)[i] = S.threshold_data_symbolic[i][0];
    REAL(z_symbolic)[i] = S.threshold_data_symbolic[i][1];
    REAL(l_symbolic)[i] = S.threshold_data_symbolic[i][2];
    INTEGER(i_symbolic)[i] = int(S.threshold_data_symbolic[i][3] + 0.5);
    INTEGER(c_symbolic)[i] = int(S.threshold_data_symbolic[i][4] + 0.5);
  }

  SEXP list_symbolic = PROTECT(allocVector(VECSXP, 5));
  SET_VECTOR_ELT(list_symbolic, 0, t_symbolic);
  SET_VECTOR_ELT(list_symbolic, 1, z_symbolic);
  SET_VECTOR_ELT(list_symbolic, 2, l_symbolic);
  SET_VECTOR_ELT(list_symbolic, 3, i_symbolic);
  SET_VECTOR_ELT(list_symbolic, 4, c_symbolic);

  SEXP names = PROTECT(allocVector(STRSXP, 5));
  SET_STRING_ELT(names, 0, mkChar("threshold"));   // name first column threshold
  SET_STRING_ELT(names, 1, mkChar("ad"));          // name second column ad, for average degree
  SET_STRING_ELT(names, 2, mkChar("lcc"));         // name third column lcc, for largest connected component
  SET_STRING_ELT(names, 3, mkChar("isolated"));    // name fourth column isolated, for number of isolated nodes
  SET_STRING_ELT(names, 4, mkChar("components"));  // name fifth column components, for number of components

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(t_symbolic);    // number of rows in symbolic edgelist

  setAttrib(list_symbolic, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_symbolic, R_RowNamesSymbol, rownames);
  setAttrib(list_symbolic, R_NamesSymbol, names);

  UNPROTECT(8);
  return list_symbolic;
}
