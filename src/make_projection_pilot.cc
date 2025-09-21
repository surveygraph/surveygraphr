#include "surveygraph.h"

#include <cctype>
#include <map>
#include <set>
#include <random>
#include <algorithm>
#include <cmath>

#include <iostream>  // TODO comment out for CRAN

#define R_NO_REMAP      // TODO comment out for CRAN, only used for debugging
#include <R.h>          // with Rprint.
#include <Rdefines.h>   //

// We can assume that rdata is an R data frame, having verified using
// is.data.frame from the calling function in R/make-projection.R.
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

// convert an R integer vector of length one to a C integer
static void rint_to_cppint(const SEXP &rval, int &cval)
{
  if(TYPEOF(rval) != INTSXP || Rf_length(rval) != 1){
    Rf_error("Expected a single integer.");
  }
  cval = INTEGER(rval)[0];
}

// convert R double vector of length one to a C double
static void rdouble_to_cppdouble(const SEXP &rval, double &cval)
{
  if(TYPEOF(rval) != REALSXP || Rf_length(rval) != 1){
    Rf_error("Expected a single double.");
  }
  cval = REAL(rval)[0];
}


static void rint_to_cppvector(const SEXP &rint, std::vector<int> &cvec)
{
  if(TYPEOF(rint) != INTSXP){
    Rf_error("Expected an R integer.");
  }

  int n = Rf_length(rint);
  cvec.resize(n);

  for(int i = 0; i < n; ++i){
    cvec[i] = INTEGER(rint)[i];
  }
}

////static void cppedgelist_to_rdf(const graph &g, SEXP &df)
//static void cppedgelist_to_rdf(std::set<edge> &edgelist, SEXP &df)
//{
//  //SEXP u_vector = PROTECT(Rf_allocVector(INTSXP, g.e));  // u column
//  //SEXP v_vector = PROTECT(Rf_allocVector(INTSXP, g.e));  // v column
//  //SEXP w_vector = PROTECT(Rf_allocVector(REALSXP, g.e)); // weight column
//
//  SEXP u_vector = PROTECT(Rf_allocVector(INTSXP, edgelist.size()));  // u column
//  SEXP v_vector = PROTECT(Rf_allocVector(INTSXP, edgelist.size()));  // v column
//  SEXP w_vector = PROTECT(Rf_allocVector(REALSXP, edgelist.size())); // weight column
//
//  //int i = 0;
//  //for(auto &it : g.network){
//  //  for(auto &jt : it.second){
//  //    if(it.first < jt.u){
//  //      INTEGER(u_vector)[i] = it.first + 1;
//  //      INTEGER(v_vector)[i] = jt.u + 1;
//  //      REAL(w_vector)[i] = jt.weight;
//  //      i += 1;
//  //    }
//  //  }  
//  //}
//
//  int i = 0;
//  for(auto &it : edgelist){
//		INTEGER(u_vector)[i] = *it.nodes.begin() + 1;
//		INTEGER(v_vector)[i] = *it.nodes.rbegin() + 1;
//		REAL(w_vector)[i] = it.weight;
//		i += 1;
//  }
//
//  SET_VECTOR_ELT(df, 0, u_vector);
//  SET_VECTOR_ELT(df, 1, v_vector);
//  SET_VECTOR_ELT(df, 2, w_vector);
//
//  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
//  SET_STRING_ELT(names, 0, Rf_mkChar("u"));            // name of first column
//  SET_STRING_ELT(names, 1, Rf_mkChar("v"));            // name of second column
//  SET_STRING_ELT(names, 2, Rf_mkChar("weight"));       // name of third column
//
//  SEXP rownames = PROTECT(Rf_allocVector(INTSXP, 2));
//  INTEGER(rownames)[0] = NA_INTEGER;                   // default entry if size below too small
//  INTEGER(rownames)[1] = -Rf_length(u_vector);         // number of rows in agent edge list
//
//  Rf_setAttrib(df, R_ClassSymbol, Rf_ScalarString(Rf_mkChar("data.frame")));
//  Rf_setAttrib(df, R_RowNamesSymbol, rownames);
//  Rf_setAttrib(df, R_NamesSymbol, names);
//
//  UNPROTECT(5);
//}

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


static void sample(std::vector<std::vector<double>> &d, const double &p, const int &seed)
{
  // TODO shouldn't this be instantiated outside of sample()?
  std::mt19937 gen(seed);
  std::uniform_real_distribution<double> unif(0, 1);

  for(int i = 0; i < d.size(); ++i)
    for(int j = 0; j < d[i].size(); ++j)
      if(unif(gen) > p) d[i][j] = NAN;
}


static void sample(std::vector<std::vector<double>> &d, const double &p)
{
  // TODO shouldn't this be instantiated outside of sample()?
  std::mt19937 gen(std::random_device{}());
  std::uniform_real_distribution<double> unif(0, 1);

  for(int i = 0; i < d.size(); ++i)
    for(int j = 0; j < d[i].size(); ++j)
      if(unif(gen) > p) d[i][j] = NAN;
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

  int layer, method, dummycode, mincompare, metric, bootreps;
  rint_to_cppint(rlayer, layer);
  rint_to_cppint(rmethod, method);
  rint_to_cppint(rmincompare, mincompare);
  rint_to_cppint(rmetric, metric);
  rint_to_cppint(rbootreps, bootreps);

  double methodval, bootval;
  rdouble_to_cppdouble(rmethodval, methodval);
  rdouble_to_cppdouble(rbootval, bootval);

  std::vector<int> bootseed;
  rint_to_cppvector(rbootseed, bootseed);

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

    surveygraph S{datasample, method, methodval, mincompare, metric};

    // Add edge list to histogram.
    for(auto &it : S.edgelist){
			eweights[it.nodes] += it.weight;
			ecounts[it.nodes] += 1;
    }

    //// Add edge list to histogram.
    //for(auto &it : S.g.network){
    //  for(auto &jt : it.second){
    //    if(it.first < jt.u){
    //      eweights[std::set<int>{it.first, jt.u}] += jt.weight;
    //      ecounts[std::set<int>{it.first, jt.u}] += 1;
    //    }
    //  }  
    //}
  }

  int n = bootreps == 1 ? 3 : 4;
  SEXP redgelist = PROTECT(Rf_allocVector(VECSXP, n));
  cppmap_to_rdf(eweights, ecounts, bootreps, redgelist);
  UNPROTECT(1);

  return redgelist;
}
