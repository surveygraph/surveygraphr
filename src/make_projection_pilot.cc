#include "surveygraph.h"

#include <cctype>

#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>

/* 

This is where we should handle data cleaning, checking for a 'group' column, type 
checking etc.j

TODO: currently assuming df is a data.frame... do not do this (why?)

Use numeric coercion to read data. If not coercable (returns NULL?) throw
error. Or, make sure every column in the dataframe is REAL, LGL, STR, INT
before it gets to here.

*/
static void df_to_cppvector(const SEXP &df, std::vector<std::vector<double>> &stmp)
{
  // check if the input is a data frame
  //if(!Rf_isFrame(df)){
  //  Rf_error("Input is not a data frame.");
  //  return;
  //}

  std::vector<std::vector<double>> surveytmp;

  SEXP check = PROTECT(Rf_allocVector(VECSXP, Rf_length(df)));
  for(int i = 0; i < Rf_length(df); ++i){
    check = VECTOR_ELT(df, i);

    if(TYPEOF(check) == STRSXP){         // convert STRSXP to double
      std::vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j){
        const char* str_value = CHAR(STRING_ELT(check, j));
        double numeric_value = std::nan("");
        if(std::isdigit(str_value[0])){
          numeric_value = atof(str_value);
        }
        coltmp.push_back(numeric_value);
      }
      surveytmp.push_back(coltmp);
    }else if(TYPEOF(check) == REALSXP){  // convert REALSXP to double
      std::vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j){
        double value = REAL(check)[j];
        if(ISNA(value)){
          value = std::nan("");
        }
        coltmp.push_back(value);
      }
      surveytmp.push_back(coltmp);
    }else if(TYPEOF(check) == INTSXP){   // convert INTSXP to double
      std::vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j){
        int value = INTEGER(check)[j];
        if(value == NA_INTEGER){
          coltmp.push_back(std::nan(""));
        }else{
          coltmp.push_back(static_cast<double>(value));
        }
      }
      surveytmp.push_back(coltmp);
    }else if(TYPEOF(check) == LGLSXP){   // convert LGLSXP to double
      std::vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j){
        int value = LOGICAL(check)[j];
        if(value == NA_LOGICAL){
          coltmp.push_back(std::nan(""));
        }else{
          coltmp.push_back(static_cast<double>(value));
        }
      }
      surveytmp.push_back(coltmp);
    }else{
      Rprintf("Warning: Unsupported type at column %d\n", i);
    }
  }

  stmp = surveytmp;

  unsigned int ncol = surveytmp.size();
  unsigned int nrow = surveytmp[0].size();

  // take the transpose, as each row is currently a user, not an item
  stmp = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));
  for(unsigned int i = 0; i < surveytmp.size(); ++i)
    for(unsigned int j = 0; j < surveytmp[i].size(); ++j)
      stmp[j][i] = surveytmp[i][j];

  UNPROTECT(1);
}

// TODO take into account optional Likert scale range
static void normalise_columns(std::vector<std::vector<double>> &s)
{
  // compute the max and min of each column
  std::vector<double> colmax(s[0].size(), -1e6);
  std::vector<double> colmin(s[0].size(),  1e6);
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
			//Rprintf("%d %d %f\n", i, j, s[i][j]);
    }
  }
}

static void cppvector_to_df(const graph &g, SEXP &c, SEXP &df)
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
				// round to four decimal places
        REAL(w_vector)[i] = int(10000.0 * jt.w) / 10000.0;
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
  cppvector_to_df(S.g_agent, c, e);

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
  cppvector_to_df(S.g_agent, c, e);

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
  cppvector_to_df(S.g_agent, c, e);

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
  cppvector_to_df(S.g_symbolic, c, e);

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
  cppvector_to_df(S.g_symbolic, c, e);

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
  cppvector_to_df(S.g_symbolic, c, e);

  UNPROTECT(1);

  return e;
}
