#include "surveygraph.h"

#include <map>
#include <random>

#define R_NO_REMAP
#include <Rinternals.h>  // SEXP
#include <R.h>           // TODO comment out for CRAN, only used for Rprintf.


// Converts and R dataframe to a nested C++ vector. We verify that `rdata` is a
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
    }
    //else{
    //  Rf_error("Non-double type at column %d of input dataframe.\n", i);
    //}
  }

  // If we're computing row similarities, producing the so-called agent layer,
  // we need to take the transpose of `data`. Otherwise leave it as is.
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
static void rint_to_cppint(const SEXP &rint, int &cint)
{
  //if(TYPEOF(rint) != INTSXP)
  //  Rf_error("Expected an R integer.");

  //if(Rf_length(rint) != 1)
  //  Rf_error("Expected an R integer vector of length 1.");

  cint = INTEGER(rint)[0];
}


// Converts an R integer vector of length one to a C++ integer of arbitrary length.
static void rint_to_cppint(const SEXP &rint, std::vector<int> &cvectorint)
{
  //if(TYPEOF(rint) != INTSXP)
  //  Rf_error("Expected an R integer.");

  cvectorint = std::vector<int>(Rf_length(rint));

  for(int i = 0; i < Rf_length(rint); ++i)
    cvectorint[i] = INTEGER(rint)[i];
}


// Converts an R double vector of length one to a C++ double.
static void rdouble_to_cppdouble(const SEXP &rdouble, double &cdouble)
{
  //if(TYPEOF(rdouble) != REALSXP)
  //  Rf_error("Expected an R double.");

  //if(Rf_length(rdouble) != 1)
  //  Rf_error("Expected an R double vector of length 1.");

  cdouble = REAL(rdouble)[0];
}


static void sample(std::vector<std::vector<double>> &d, const double &p, const int &seed)
{
  // TODO shouldn't this be instantiated outside of sample()?
  std::mt19937 gen(seed);
  std::uniform_real_distribution<double> unif(0, 1);

  for(unsigned int i = 0; i < d.size(); ++i)
    for(unsigned int j = 0; j < d[i].size(); ++j)
      if(unif(gen) > p) d[i][j] = NAN;
}


static void sample(std::vector<std::vector<double>> &d, const double &p)
{
  // TODO shouldn't this be instantiated outside of sample()?
  std::mt19937 gen(std::random_device{}());
  std::uniform_real_distribution<double> unif(0, 1);

  for(unsigned int i = 0; i < d.size(); ++i)
    for(unsigned int j = 0; j < d[i].size(); ++j)
      if(unif(gen) > p) d[i][j] = NAN;
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
  INTEGER(rownames)[0] = NA_INTEGER;
  INTEGER(rownames)[1] = -Rf_length(u_vector);

  Rf_setAttrib(df, R_ClassSymbol, Rf_ScalarString(Rf_mkChar("data.frame")));
  Rf_setAttrib(df, R_RowNamesSymbol, rownames);
  Rf_setAttrib(df, R_NamesSymbol, names);

  UNPROTECT(6);
}


// Basic checks have been carried out in the calling R file, R/make-projection.R.
SEXP rmake_projection(
  SEXP rdata,         // dataframe containing survey
  SEXP rlayer,        // layer flag, agent or symbolic
  SEXP rmethod,       // sparsification method, lcc, avgdegree, similarity
  SEXP rmethodval,    // method value, utility variable
  SEXP rcomparisons,  // minimum number of comparisons
  SEXP rmetric,       // similarity metric
  SEXP rbootreps,     // number of bootstrap repetitions
  SEXP rbootval,      // resampling probability for bootstrapping
  SEXP rbootseed      // resampling seed, used when testing
){

  int layer, method, comparisons, metric, bootreps;
  rint_to_cppint(rlayer, layer);
  rint_to_cppint(rmethod, method);
  rint_to_cppint(rcomparisons, comparisons);
  rint_to_cppint(rmetric, metric);
  rint_to_cppint(rbootreps, bootreps);

  double methodval, bootval;
  rdouble_to_cppdouble(rmethodval, methodval);
  rdouble_to_cppdouble(rbootval, bootval);

  std::vector<int> bootseed;
  rint_to_cppint(rbootseed, bootseed);

  std::vector<std::vector<double>> data;
  rdf_to_cppvector(rdata, layer, data);

  std::map<std::set<int>, int> ecounts;
  std::map<std::set<int>, double> eweights;

  for(int i = 0; i < bootreps; ++i){
    std::vector<std::vector<double>> datasample = data;

    if(bootseed.size() == 0)
      sample(datasample, bootval);
    else
      sample(datasample, bootval, bootseed[i]);

    surveygraph S{datasample, method, methodval, comparisons, metric};

    for(auto &it : S.edgelist){
      eweights[it.nodes] += it.weight;
      ecounts[it.nodes] += 1;
    }
  }

  int n = bootreps == 1 ? 3 : 4;
  SEXP redgelist = PROTECT(Rf_allocVector(VECSXP, n));
  cppmap_to_rdf(eweights, ecounts, bootreps, redgelist);
  UNPROTECT(1);

  return redgelist;
}
