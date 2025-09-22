#include "surveygraph.h"

#define R_NO_REMAP     // TODO comment out for CRAN, only used for debugging 
#include <R.h>         // with Rprint.
#include <Rdefines.h>  //


// Converts and R dataframe to a nested C++ vector. We verify that rdata is a
// dataframe elsewhere.
static void rdf_to_cppvector(
  const SEXP &rdata, 
  const int &layer, 
  std::vector<std::vector<double>> &data
){
  data = std::vector<std::vector<double>>{};

  SEXP dummy = PROTECT(Rf_allocVector(VECSXP, Rf_length(rdata)));

  // Columns i of the dataframe `rdata` become rows i of `data`.
  for(int i = 0; i < Rf_length(rdata); ++i){
    dummy = VECTOR_ELT(rdata, i);

    if(TYPEOF(dummy) == REALSXP){
      std::vector<double> coltmp;
      for(int j = 0; j < Rf_length(dummy); ++j){
        double value = REAL(dummy)[j];
        if(ISNA(value)){
          value = std::nan("");
        }
        coltmp.push_back(value);
      }
      data.push_back(coltmp);
    }else{
      Rf_error("Non-double type at column %d of input dataframe.\n", i);
    }
  }

  // If we're computing row similarities, producing the so-called agent layer,
  // we need to take the transpose of `data`. Otherwise, leave as is.
  if(layer == 0){
    unsigned int ncol = data.size();
    unsigned int nrow = data[0].size();

    std::vector<std::vector<double>> dummy = data;
    data = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));
    for(unsigned int i = 0; i < ncol; ++i)
      for(unsigned int j = 0; j < nrow; ++j)
        data[j][i] = dummy[i][j];
  }

  UNPROTECT(1);
}

// Converts an R integer vector of length one to a C++ integer.
static void rint_to_cppint(const SEXP &rval, int &cval)
{
  if(TYPEOF(rval) != INTSXP || Rf_length(rval) != 1){
    Rf_error("Expected a single integer.");
  }
  cval = INTEGER(rval)[0];
}

// Converts an R double vector of length one to a C++ double.
static void rdouble_to_cppdouble(const SEXP &rval, double &cval)
{
  if(TYPEOF(rval) != REALSXP || Rf_length(rval) != 1){
    Rf_error("Expected a single double.");
  }
  cval = REAL(rval)[0];
}


// Converts a nested C++ vector to an R dataframe.
static void cppvector_to_rdf(
  const std::vector<std::vector<int>> &profile,
  SEXP &df
){
  SEXP t_vector = PROTECT(Rf_allocVector(REALSXP, profile.size()));  // threshold 
  SEXP e_vector = PROTECT(Rf_allocVector(INTSXP,  profile.size()));  // edge count
  SEXP l_vector = PROTECT(Rf_allocVector(INTSXP,  profile.size()));  // largest connected component
  SEXP s_vector = PROTECT(Rf_allocVector(INTSXP,  profile.size()));  // singleton count
  SEXP c_vector = PROTECT(Rf_allocVector(INTSXP,  profile.size()));  // component count

  for(int i = 0; i < profile.size(); ++i){
    int idash = profile.size() - 1 - i;
    REAL(t_vector)[i] = i / double(profile.size() - 1);
    INTEGER(l_vector)[i] = profile[idash][0];
    INTEGER(e_vector)[i] = profile[idash][1]; 
    INTEGER(c_vector)[i] = profile[idash][2]; 
    INTEGER(s_vector)[i] = profile[idash][3]; 
	}

  SET_VECTOR_ELT(df, 0, t_vector);
  SET_VECTOR_ELT(df, 1, l_vector);
  SET_VECTOR_ELT(df, 2, e_vector);
  SET_VECTOR_ELT(df, 3, c_vector);
  SET_VECTOR_ELT(df, 4, s_vector);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));
  SET_STRING_ELT(names, 0, Rf_mkChar("threshold"));
  SET_STRING_ELT(names, 1, Rf_mkChar("lcc")); 
  SET_STRING_ELT(names, 2, Rf_mkChar("edges"));
  SET_STRING_ELT(names, 3, Rf_mkChar("components"));
  SET_STRING_ELT(names, 4, Rf_mkChar("isolated"));

  SEXP rownames = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;
  INTEGER(rownames)[1] = -Rf_length(t_vector);

  Rf_setAttrib(df, R_ClassSymbol, Rf_ScalarString(Rf_mkChar("data.frame")));
  Rf_setAttrib(df, R_RowNamesSymbol, rownames);
  Rf_setAttrib(df, R_NamesSymbol, names);

  UNPROTECT(7);
}


SEXP rmake_threshold_profile(
  SEXP rdata,        // dataframe containing survey
  SEXP rlayer,       // layer flag, agent or symbolic
  SEXP rmincompare,  // minimum comparisons
  SEXP rmetric,      // similarity metric
  SEXP rcount        // number of values in profile
){
  int layer, mincompare, metric, count;
  rint_to_cppint(rlayer, layer);
  rint_to_cppint(rmincompare, mincompare);
  rint_to_cppint(rmetric, metric);
  rint_to_cppint(rcount, count);

  std::vector<std::vector<double>> data;
  rdf_to_cppvector(rdata, layer, data);

  surveygraph S{data, mincompare, metric, count};

  SEXP rprofile = PROTECT(Rf_allocVector(VECSXP, 5));
  cppvector_to_rdf(S.profile, rprofile);
  UNPROTECT(1);

  return rprofile;
}
