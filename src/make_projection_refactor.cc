#include "surveygraph.h"

#include <cctype>
#include <algorithm>

#include <iostream> // TODO temporary

#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>

// We can assume that rdata is an R data frame, having verified using is.data.frame 
// from the calling function in R/make-projection.R.
static void rdf_to_cppvector(const SEXP &rdata, std::vector<std::vector<double>> &data)
{
  data = std::vector<std::vector<double>>{};

  SEXP check = PROTECT(Rf_allocVector(VECSXP, Rf_length(rdata)));

  // iterate over columns of dataframe... index by j?
  for(int i = 0; i < Rf_length(rdata); ++i){
    check = VECTOR_ELT(rdata, i);

    // TODO simply verify that it's real, throw an error if it's anything else

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
      data.push_back(coltmp);
    }else if(TYPEOF(check) == REALSXP){  // convert REALSXP to double
      std::vector<double> coltmp;
      for(int j = 0; j < Rf_length(check); ++j){
        double value = REAL(check)[j];
        if(ISNA(value)){
          value = std::nan("");
        }
        coltmp.push_back(value);
        //Rprintf("%d %d %f\n", i, j, value);
      }
      data.push_back(coltmp);
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
      data.push_back(coltmp);
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
      data.push_back(coltmp);
    }else{
      Rprintf("Warning: unsupported type at column %d\n", i);
    }
  }

  unsigned int ncol = data.size();     // this is m
  unsigned int nrow = data[0].size();  // this is n

  // take the transpose, as each row is currently an item, not a respondent
  std::vector<std::vector<double>> dummy = data;
  data = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));
  for(unsigned int i = 0; i < ncol; ++i)
    for(unsigned int j = 0; j < nrow; ++j)
      data[j][i] = dummy[i][j];

  UNPROTECT(1);
}

// convert an R integer vector of length one to a C integer
// TODO you haven't thought about NA entries
static void rint_to_cppint(const SEXP &rval, int &cval)
{
  if(TYPEOF(rval) != INTSXP || Rf_length(rval) != 1){
    Rf_error("Expected a single integer.");
  }
  cval = INTEGER(rval)[0];
}

// convert R double vector of length one to a C double
// TODO you haven't thought about NA entries
static void rdouble_to_cppdouble(const SEXP &rval, double &cval)
{
  if(TYPEOF(rval) != REALSXP || Rf_length(rval) != 1){
    Rf_error("Expected a single double.");
  }
  cval = REAL(rval)[0];
}

// convert R list containing R double vectors, of length either 1 or 2, to a C++ vector
// TODO you haven't thought about NA entries
static void rlist_to_cppvector(const SEXP &rlist, std::vector<std::vector<double>> &cvec)
{
  if(TYPEOF(rlist) != VECSXP){
    Rf_error("Expected a list.");
  }

  int n = Rf_length(rlist);
  cvec.resize(n);

  for(int i = 0; i < n; ++i){
    SEXP rvec = VECTOR_ELT(rlist, i);
    if(TYPEOF(rvec) != REALSXP){
      Rf_error("Expected a numeric vector inside the list.");
    }

    int len = Rf_length(rvec);
    std::vector<double> temp(len);
    
    for(int j = 0; j < len; ++j){
      temp[j] = REAL(rvec)[j];
    }
    
    cvec[i] = temp;
  }
}

/*

This function cleans the contents of `data`. It checks that
- at least one row and column
- all double precision
- all columns have the same dimension
- either a finite value or NaN

*/
static void clean_data(std::vector<std::vector<double>> &data){
  //Rprintf("data before :\n");
  //for(int i = 0; i < data.size(); ++i){
  //  for(int j = 0; j < data[i].size(); ++j) Rprintf("%10f ", data[i][j]);
  //  Rprintf("\n");
  //}
  //Rprintf("\n");

  // Prepare a temporary container for cleaned data.
  std::vector<std::vector<double>> cleandata;

  // Normalise entries each column to the range 0 to 1
  for(int j = 0; j < data[0].size(); ++j){
    // Find bounds on entries in column j. 
    // TODO what if dhi and or dlo are nan? or the same number?
    double dhi = data[0][j];
    double dlo = data[0][j];
    for(int i = 1; i < data.size(); ++i){
      if(data[i][j] > dhi) dhi = data[i][j];
      if(data[i][j] < dlo) dlo = data[i][j];
    }
    
    std::vector<double> normalised_column(data.size());
    for(int i = 0; i < data.size(); ++i){
      if(dhi != dlo){
        if(!std::isnan(data[i][j])){
          // TODO what if dhi and dlo are the same number?
          normalised_column[i] = (data[i][j] - dlo) / (dhi - dlo);
        }else{
          normalised_column[i] = std::nan("");
        }
      }else{
        normalised_column[i] = std::nan("");
      }
    }
    cleandata.push_back(normalised_column);
  }

  // Replace original data with the cleaned version. Take the transpose of
  // cleandata, ncol and nrow are the resulting column and row count.
  unsigned int ncol = cleandata.size();
  unsigned int nrow = cleandata[0].size();

  data = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));
  for(unsigned int i = 0; i < cleandata.size(); ++i){
    for(unsigned int j = 0; j < cleandata[i].size(); ++j){
      data[j][i] = cleandata[i][j];
    }
  }
}

static void cppedgelist_to_rdf(const graph &g, SEXP &df)
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
        REAL(w_vector)[i] = int(10000.0 * jt.w) / 10000.0;
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

// Basic checks have been carried out in the calling R file, R/make-projection.R.
SEXP rmake_projection(
  SEXP rdata,        // dataframe containing survey
  SEXP rlayer,       // layer flag, agent or symbolic
  SEXP rmethod,      // sparsification method, lcc, average_degree, raw_similarity
  SEXP rmethodval,   // method value, utility variable
  SEXP rmincompare,  // minimum comparisons
  SEXP rmetric       // similarity metric
){

  std::vector<std::vector<double>> data;
  rdf_to_cppvector(rdata, data);

  int layer, method, dummycode, mincompare, metric;
  rint_to_cppint(rlayer, layer);
  rint_to_cppint(rmethod, method);
  rint_to_cppint(rmincompare, mincompare);
  rint_to_cppint(rmetric, metric);

  double methodval;
  rdouble_to_cppdouble(rmethodval, methodval);

  clean_data(data);

  // Everything is done inside the constructor, creating S.g_dummy
  surveygraph S{
    data, 
    layer, 
    method, 
    methodval, 
    mincompare, 
    metric
  };

  SEXP redgelist = PROTECT(Rf_allocVector(VECSXP, 3));
  cppedgelist_to_rdf(S.g_dummy, redgelist);
  UNPROTECT(1);

  return redgelist;
}
