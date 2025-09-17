#include "surveygraph.h"

#include <cctype>
#include <map>
#include <set>
#include <random>
#include <algorithm>
#include <cmath>

#include <iostream> // TODO temporary

#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>

// We can assume that rdata is an R data frame, having verified using is.data.frame 
// from the calling function in R/make-projection.R.
static void rdf_to_cppvector(const SEXP &rdata, const int &layer, std::vector<std::vector<double>> &data)
{
  data = std::vector<std::vector<double>>{};

  SEXP check = PROTECT(Rf_allocVector(VECSXP, Rf_length(rdata)));

  // Iterate over columns of dataframe `rdata`, so i indexes the row of
  // `data`, so that `data` is the transpose of `rdata`.
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

  // If we're computing row similarities, producing the so-called agent layer,
  // we need to take the transpose of data. If we're computing column
  // similarities, producing the so-called symbolic layer, we leave as is.
  if(layer == 0){
    std::vector<std::vector<double>> dummy = data;
    data = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));
    for(unsigned int i = 0; i < ncol; ++i)
      for(unsigned int j = 0; j < nrow; ++j)
        data[j][i] = dummy[i][j];
  }

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


static void scale_columns(std::vector<std::vector<double>> &data){
  // For each column j of data...
  for(int j = 0; j < data[0].size(); ++j){
    // find bounds on entries in column j, if there are entries that aren't NaNs...
    double dhi = NAN;
    double dlo = NAN;
    for(int i = 0; i < data.size(); ++i){
      if(!isnan(data[i][j])){
        if(!isnan(dhi)){
          if(data[i][j] > dhi) dhi = data[i][j];
        }else{
          dhi = data[i][j];
        }

        if(!isnan(dlo)){
          if(data[i][j] < dlo) dlo = data[i][j];
        }else{
          dlo = data[i][j];
        }
      }
    }
    
    // and scale non-NaN entries to the range 0 to 1
    for(int i = 0; i < data.size(); ++i){
      if(!std::isnan(data[i][j])){
        if(dhi > dlo)
          data[i][j] = (data[i][j] - dlo) / (dhi - dlo);
        else
          data[i][j] = 0.5;
      }
    }
  }

  //Rprintf("data after :\n");
  //for(int i = 0; i < data.size(); ++i){
  //  for(int j = 0; j < data[i].size(); ++j) Rprintf("%10f ", data[i][j]);
  //  Rprintf("\n");
  //}
  //Rprintf("\n");
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
        REAL(w_vector)[i] = jt.w;
        i += 1;
      }
    }  
  }

  SET_VECTOR_ELT(df, 0, u_vector);
  SET_VECTOR_ELT(df, 1, v_vector);
  SET_VECTOR_ELT(df, 2, w_vector);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("u"));            // name of first column
  SET_STRING_ELT(names, 1, Rf_mkChar("v"));            // name of second column
  SET_STRING_ELT(names, 2, Rf_mkChar("weight"));       // name of third column (required by igraph)

  SEXP rownames = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                   // default entry if size below too small
  INTEGER(rownames)[1] = -Rf_length(u_vector);         // number of rows in agent edge list

  Rf_setAttrib(df, R_ClassSymbol, Rf_ScalarString(Rf_mkChar("data.frame")));
  Rf_setAttrib(df, R_RowNamesSymbol, rownames);
  Rf_setAttrib(df, R_NamesSymbol, names);

  UNPROTECT(5);
}

static void cppmap_to_rdf(
  std::map<std::set<int>, double> &e, 
  std::map<std::set<int>, int> &c, 
  const int &ctotal,
  SEXP &df)
{
  unsigned int len = e.size();
  SEXP u_vector = PROTECT(Rf_allocVector(INTSXP, len));  // u column
  SEXP v_vector = PROTECT(Rf_allocVector(INTSXP, len));  // v column
  SEXP w_vector = PROTECT(Rf_allocVector(REALSXP, len)); // weight column
  SEXP f_vector = PROTECT(Rf_allocVector(REALSXP, len)); // frequency column

  int i = 0;
  for(auto &it : e){
    INTEGER(u_vector)[i] = *it.first.begin() + 1;
    INTEGER(v_vector)[i] = *it.first.rbegin() + 1;
    REAL(w_vector)[i] = it.second / double(c[it.first]);
    REAL(f_vector)[i] = c[it.first] / double(ctotal);
    i += 1;
  }

  SET_VECTOR_ELT(df, 0, u_vector);
  SET_VECTOR_ELT(df, 1, v_vector);
  SET_VECTOR_ELT(df, 2, w_vector);
  if(Rf_length(df) == 4) SET_VECTOR_ELT(df, 3, f_vector);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, Rf_length(df)));
  SET_STRING_ELT(names, 0, Rf_mkChar("u"));
  SET_STRING_ELT(names, 1, Rf_mkChar("v"));
  SET_STRING_ELT(names, 2, Rf_mkChar("weight"));
  if(Rf_length(names) == 4) SET_STRING_ELT(names, 3, Rf_mkChar("freq"));

  SEXP rownames = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                   // default entry if size below too small
  INTEGER(rownames)[1] = -Rf_length(u_vector);         // number of edges in edge list

  Rf_setAttrib(df, R_ClassSymbol, Rf_ScalarString(Rf_mkChar("data.frame")));
  Rf_setAttrib(df, R_RowNamesSymbol, rownames);
  Rf_setAttrib(df, R_NamesSymbol, names);

  UNPROTECT(6);
}


static void sample(
  std::vector<vector<double>> &d, 
  const double &p, 
  const int &seed, 
  const int &seedval
){
  // TODO shouldn't this be instantiated outside of sample()?
  std::mt19937 gen(std::random_device{}());
  if(seed) gen = std::mt19937(seedval);

  //gen = std::mt19937(2);
  std::vector<vector<double>> r = d;

  std::uniform_real_distribution<double> unif(0, 1);

  for(int i = 0; i < d.size(); ++i){
    for(int j = 0; j < d[i].size(); ++j){
      double x = unif(gen);
      r[i][j] = x;
      if(x > p) d[i][j] = NAN;
    }
  }

  //Rprintf("=============================================================\n");
  //for(int i = 0; i < r.size(); ++i){
  //  for(int j = 0; j < r[i].size(); ++j){
  //    Rprintf("%8f ", r[i][j]);
  //  }
  //  Rprintf("\n");
  //}
  //Rprintf("\n");

  //for(int i = 0; i < d.size(); ++i){
  //  for(int j = 0; j < d[i].size(); ++j){
  //    Rprintf("%-8f ", d[i][j]);
  //  }
  //  Rprintf("\n");
  //}
}

// Basic checks have been carried out in the calling R file, R/make-projection.R.
SEXP rmake_projection(
  SEXP rdata,        // dataframe containing survey
  SEXP rlayer,       // layer flag, agent or symbolic
  SEXP rmethod,      // sparsification method, lcc, average_degree, raw_similarity
  SEXP rmethodval,   // method value, utility variable
  SEXP rmincompare,  // minimum comparisons
  SEXP rmetric,      // similarity metric
  SEXP rbootreps,    // number of bootstrap repetitions
  SEXP rbootval,     // resampling probability for bootstrapping
  SEXP rbootseed     // resampling seed, used when testing
){

  int layer, method, dummycode, mincompare, metric, bootreps, bootseed;
  rint_to_cppint(rlayer, layer);
  rint_to_cppint(rmethod, method);
  rint_to_cppint(rmincompare, mincompare);
  rint_to_cppint(rmetric, metric);
  rint_to_cppint(rbootreps, bootreps);
  rint_to_cppint(rbootseed, bootseed);

  double methodval, bootval;
  rdouble_to_cppdouble(rmethodval, methodval);
  rdouble_to_cppdouble(rbootval, bootval);

  std::vector<std::vector<double>> data;
  rdf_to_cppvector(rdata, layer, data);

  std::map<std::set<int>, int> ecounts;
  std::map<std::set<int>, double> eweights;

  for(int i = 0; i < bootreps; ++i){
    std::vector<std::vector<double>> datasample = data;

    if(bootval < 1){
      sample(datasample, bootval, bootseed, i);
    }

    surveygraph S{datasample, method, methodval, mincompare, metric};

    for(auto &it : S.g.network){
      for(auto &jt : it.second){
        if(it.first < jt.u){
          eweights[std::set<int>{it.first, jt.u}] += jt.w;
          ecounts[std::set<int>{it.first, jt.u}] += 1;
        }
      }  
    }
  }

  int n = bootreps == 1 ? 3 : 4;
  SEXP redgelist = PROTECT(Rf_allocVector(VECSXP, n));
  cppmap_to_rdf(eweights, ecounts, bootreps, redgelist);
  UNPROTECT(1);

  return redgelist;
}
