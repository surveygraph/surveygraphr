#include "surveygraph.h"

#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>

/* 

This is where we should handle data cleaning, checking for a 'group' column, type 
checking etc.

FIXME wouldn't it be easiest to assume that every column provided is to be used
to find similarity? ie drop group column before we input it

TODO: currently assuming df is a data.frame... do not do this

Use numeric coercion to read deta. If not coercable (returns NULL?) skip column.

*/
static void df_to_cppvector(const SEXP &df, std::vector<vector<double>> &stmp)
{
  vector<vector<double>> surveytmp;

  SEXP check = PROTECT(Rf_allocVector(VECSXP, Rf_length(df)));
  for(int i = 0; i < Rf_length(df); ++i){
    check = VECTOR_ELT(df, i);

    Rprintf("checking types, i : %d (%d)\n", i, TYPEOF(check));

    if(TYPEOF(check) == STRSXP) {
      vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j) {
        const char* str_value = CHAR(STRING_ELT(check, j));
        // Coerce string to number or set to NaN
        double numeric_value = std::nan("");  // default to NaN
        if(std::isdigit(str_value[0])) {      // simple check for numeric string
          numeric_value = atof(str_value);    // convert string to double
        }
        coltmp.push_back(numeric_value);
        Rprintf("%f\n", numeric_value);
      }
      surveytmp.push_back(coltmp);
    }else if(TYPEOF(check) == REALSXP){
      vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j){
        coltmp.push_back(REAL(check)[j]);
        Rprintf("%d %d %f\n", i, j, REAL(check)[j]);
        //Rprintf("have you found an NA? %d %d %f\n", i, j, REAL(check)[j]);
        //Rprintf("%d\n", ISNA(REAL(check)[j]));
        //if(ISNA(check)){
        //  Rprintf("you've found an NA: %d %d\n", i, j);
        //}
      }
      surveytmp.push_back(coltmp);
    }else if(TYPEOF(check) == INTSXP){
      vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j){
        coltmp.push_back(double(INTEGER(check)[j]));
      }
      surveytmp.push_back(coltmp);
    }else if(TYPEOF(check) == LGLSXP){
      vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j){
        int value = LOGICAL(check)[j];
        if(value == NA_LOGICAL){
          coltmp.push_back(std::nan(""));  // Set NA logicals to NaN
        }else{
          coltmp.push_back(static_cast<double>(value));  // Coerce logical to double
        }
      }
      surveytmp.push_back(coltmp);
    }else{
      // TODO set bools to numerical value
      /*
      TODO: ouput a warning message if df contains none of the above types
      */
    }
  }

  unsigned int ncol = surveytmp.size();
  unsigned int nrow = surveytmp[0].size();

  // TODO take the transpose, but I forget why...
  stmp = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));
  for(unsigned int i = 0; i < surveytmp.size(); ++i)
    for(unsigned int j = 0; j < surveytmp[i].size(); ++j)
      stmp[j][i] = surveytmp[i][j];

  UNPROTECT(1);
}

// TODO take into account optional Likert scale range
static void normalise_columns(std::vector<vector<double>> &s)
{
  // compute the max and min of each column
  vector<double> colmax(s[0].size(), -1e6);
  vector<double> colmin(s[0].size(),  1e6);
  for(unsigned int j = 0; j < s[0].size(); ++j){
    for(unsigned int i = 0; i < s.size(); ++i){
      if(s[i][j] > colmax[j]) colmax[j] = s[i][j];
      if(s[i][j] < colmin[j]) colmin[j] = s[i][j];
    }
  }

  // map column entries to the interval to [-1, 1]
  for(unsigned int j = 0; j < s[0].size(); ++j){
    double m = 2 / (colmax[j] - colmin[j]);
    double b = -(colmax[j] + colmin[j]) / (colmax[j] - colmin[j]);
    for(unsigned int i = 0; i < s.size(); ++i){
      s[i][j] = m * s[i][j] + b;
    }
  }
}

static void vectors_to_df(const graph &g, SEXP &c, SEXP &df)
{
  SEXP u_vector = PROTECT(Rf_allocVector(INTSXP, g.e));  // u column
  SEXP v_vector = PROTECT(Rf_allocVector(INTSXP, g.e));  // v column
  SEXP w_vector = PROTECT(Rf_allocVector(REALSXP, g.e)); // weight column

  int i = 0;
  for(auto &it : g.network){
    for(auto &jt : it.second){
      if(it.first < jt.u){
        INTEGER(u_vector)[i] = it.first + 1;
        INTEGER(v_vector)[i] = jt.u + 1;
        REAL(w_vector)[i] = int(100.0 * jt.w) / 100.0;
        if(INTEGER(c)[0] == 0) REAL(w_vector)[i] += 1.0;
        i += 1;
      }
    }
  }

  SET_VECTOR_ELT(df, 0, u_vector);
  SET_VECTOR_ELT(df, 1, v_vector);
  SET_VECTOR_ELT(df, 2, w_vector);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("u"));            // name first column u
  SET_STRING_ELT(names, 1, Rf_mkChar("v"));            // name second column v
  SET_STRING_ELT(names, 2, Rf_mkChar("weight"));       // name third column weight, required by igraph

  SEXP rownames = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                   // default entry if size below too small
  INTEGER(rownames)[1] = -Rf_length(u_vector);         // number of rows in agent edge list

  Rf_setAttrib(df, R_ClassSymbol, Rf_ScalarString(Rf_mkChar("data.frame")));
  Rf_setAttrib(df, R_RowNamesSymbol, rownames);
  Rf_setAttrib(df, R_NamesSymbol, names);

  UNPROTECT(5);
}

/*

Recall that the arguments to these routines, which are called by
make-projection.R, are 
  1. df, the survey in dataframe format
  2. mvalue, the method value
  3. c, the centering flat, which centres the similarity weights around zero 
  4. sim_metric, the similarity metric

*/
SEXP rmake_proj_agent_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 0, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_lcc();

  SEXP e = PROTECT(Rf_allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_agent_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 1, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_ad();

  SEXP e = PROTECT(Rf_allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_agent_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 2, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_similar();

  SEXP e = PROTECT(Rf_allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_symbolic_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 0, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_lcc();

  SEXP e = PROTECT(Rf_allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_symbolic_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 1, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_ad();

  SEXP e = PROTECT(Rf_allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_symbolic_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 2, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_similar();

  SEXP e = PROTECT(Rf_allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(1);

  return e;
}
