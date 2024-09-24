#include "surveygraph.h"

#include <cctype>
#include <algorithm>
#include <unordered_set>

#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>

// We can assume that rdata is an R data frame, having verified using is.data.frame 
// from the calling function in R/make-projection.R.
static void rdf_to_cppvector(const SEXP &rdata, std::vector<std::vector<double>> &data)
{
  data = std::vector<std::vector<double>>{};

  SEXP check = PROTECT(Rf_allocVector(VECSXP, Rf_length(rdata)));
  for(int i = 0; i < Rf_length(rdata); ++i){
    check = VECTOR_ELT(rdata, i);

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
      Rprintf("Warning: Unsupported type at column %d\n", i);
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

This function cleans the contents of `data`. It works as follows.
  - Verify that likert.size() equals data[0].size().
  - Then, for each column j in `data`, what we do depends on the following
  cases.
    1. likert[j][0] is nan and dummycode is 0. In this case, we find the
    upper and lower bounds on the values in data[i][j], for 0 \leq i <
    data.size(), calling these bounds dhi and dlo, respectively. We then
    shift and scale the elements of column i of data to the interval -1 to 1.
    This follows the linear map 
      data[i][j] = (2 / (dhi - dlo)) * data[i][j] + (dlo + dhi) / (dlo - dhi),
    such that the new upper and lower bounds on column j are 1 and -1,
    respectively.
    2. likert[j][0] is nan and `dummycode` is 1. In this case, we use dummy
    coding, or one-hot encoding, of every entry found in column j of `data`.
    That is, if we find 3 distinct entries in column j, the cleaned version
    of `data` contains 3 columns in the place of column j with entries either
    0 or 1.
    3. likert[j][0] and likert[j][1] are real numerical values, and
    `dummycode` is 1. In this case, every value inside the range specified by
    likert[j] is mapped to the interval -1 and 1, as per case 1, and every value
    outside this range is dummy coded, as per case 2.
  - In the step above, we create a temporary array `cleandata`, with additional
  columns if we've allowed dummy coding. The final step is to overwrite `data`
  with the cleaned version.

*/
static void clean_data(std::vector<std::vector<double>> &data, const std::vector<std::vector<double>> &likert, const int &dummycode)
{
  // Step 1: Verify likert.size() equals data[0].size()
  if(likert.size() != data[0].size()){
    Rf_error("Mismatch between likert size and data column count.");
  }

  Rprintf("data before :\n");
  for(int i = 0; i < data.size(); ++i){
    for(int j = 0; j < data[i].size(); ++j){
      Rprintf("%f ", data[i][j]);
    }
    Rprintf("\n");
  }
  Rprintf("\n");

  Rprintf("likert specification :\n");
  for(int i = 0; i < likert.size(); ++i){
    for(int j = 0; j < likert[i].size(); ++j){
      Rprintf("%f ", likert[i][j]);
    }
    Rprintf("\n");
  }
  Rprintf("\n");

  Rprintf("dummycode : %d\n", dummycode);
  Rprintf("\n");

  // Step 2: Prepare a temporary container for cleaned data
  std::vector<std::vector<double>> cleandata;
  
  // Step 3: Process each column in data
  for(size_t j = 0; j < data[0].size(); ++j){
    // If likert[j][0] is NaN
    if(std::isnan(likert[j][0])){
      if(dummycode == 0){
        // Find bounds on entries in column j.
        double dhi = data[0][j];
        double dlo = data[0][j];
        for(int i = 1; i < data.size(); ++i){
          if(data[i][j] > dhi) dhi = data[i][j];
          if(data[i][j] < dlo) dlo = data[i][j];
        }
        
        std::vector<double> normalised_column(data.size());
        for(size_t i = 0; i < data.size(); ++i){
          if(dhi != dlo){
            normalised_column[i] = (2 / (dhi - dlo)) * data[i][j] + (dlo + dhi) / (dlo - dhi);
          }else{
            normalised_column[i] = 0;
          }
        }
        cleandata.push_back(normalised_column);
      }else{
        // Case 2: Apply dummy coding (one-hot encoding)
        std::unordered_set<double> distinct_values;
        for(const auto &row : data){
          distinct_values.insert(row[j]);
        }
        for(double value : distinct_values){
          std::vector<double> dummy_column(data.size(), 0.0);
          for(size_t i = 0; i < data.size(); ++i){
            if(data[i][j] == value){
              dummy_column[i] = 1.0;
            }
          }
          cleandata.push_back(dummy_column);
        }
      }
    }else{
      // Case 3: Normalise within the range [likert[j][0], likert[j][1]], dummy code outside range
      double lower = likert[j][0];
      double upper = likert[j][1];
      std::vector<double> normalised_column(data.size());

      // Step 3a: Gather distinct values outside the likert range
      std::unordered_set<double> outside_values;
      for(size_t i = 0; i < data.size(); ++i){
        if(data[i][j] >= lower && data[i][j] <= upper){
          normalised_column[i] = (2 / (upper - lower)) * (data[i][j] - lower) - 1;
        }else if(dummycode == 1){
          outside_values.insert(data[i][j]);
        }
      }
      cleandata.push_back(normalised_column);

      // Step 3b: Apply dummy coding to distinct values outside the range
      for(double value : outside_values){
        std::vector<double> dummy_column(data.size(), 0.0);
        for(size_t i = 0; i < data.size(); ++i){
          if(data[i][j] == value){
            dummy_column[i] = 1.0;
          }
        }
        cleandata.push_back(dummy_column);
      }
    }
  }

  // Step 4: Replace original data with the cleaned version
  unsigned int ncol = data.size();
  unsigned int nrow = data[0].size();

  //data = cleandata;

  // take the transpose, as each row is currently a user, not an item
  data = std::vector<std::vector<double>>(ncol, std::vector<double>(nrow));
  for(unsigned int i = 0; i < cleandata.size(); ++i){
    for(unsigned int j = 0; j < cleandata[i].size(); ++j){
      data[j][i] = cleandata[i][j];
      Rprintf("%9f ", data[i][j]);
    }
    Rprintf("\n");
  }

  Rprintf("data after :\n");
  for(int i = 0; i < data.size(); ++i){
    for(int j = 0; j < data[i].size(); ++j){
      Rprintf("%f ", data[i][j]);
    }
    Rprintf("\n");
  }
  Rprintf("\n");
}

static void cppvector_to_rdf(const graph &g, SEXP &c, SEXP &df)
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

// Basic checks have been carried out in the calling R file, R/make-projection.R.
SEXP rmake_projection(
  SEXP rdata,       // dataframe containing survey
  SEXP rlayer,      // layer flag, agent or symbolic
  SEXP rmethod,     // sparsification method, lcc, average_degree, raw_similarity
  SEXP rmethodval,  // method value, utility variable
  SEXP rcentre,     // centering flag, 0 or 1
  SEXP rlikert,     // likert scale specification
  SEXP rdummycode,  // dummy coding boolean
  SEXP rmincomps,   // minimum comparisons
  SEXP rsimilarity) // similarity metric
{
  /* 
  The following methods convert SEXP objects to the relevant C++ types.
  We assume the following R types and C++ conversions
    - rdata is an R dataframe, to convert to C++ vector<vector<double>>
    - rlayer, rmethod, rcentre, rdummycode, rmincomps and rsimilarity are R
    integers, meaning integer vectors of length one, to convert to C++ ints.
    - rmethodval is an R double, meaning a double vector of length one, to
    convert to C++ double. 
    - rlikert is an R list of length m. Each entry is an R double vector of 
    length either two or one, to be converted to C++ vector<vector<double>>.
  */

  std::vector<std::vector<double>> data;
  rdf_to_cppvector(rdata, data);

  int layer, method, centre, dummycode, mincomps, similarity;
  rint_to_cppint(rlayer, layer);
  rint_to_cppint(rmethod, method);
  rint_to_cppint(rcentre, centre);
  rint_to_cppint(rdummycode, dummycode);
  rint_to_cppint(rmincomps, mincomps);
  rint_to_cppint(rsimilarity, similarity);

  double methodval;
  rdouble_to_cppdouble(rmethodval, methodval);

  std::vector<std::vector<double>> likert;
  rlist_to_cppvector(rlikert, likert);

  clean_data(data, likert, dummycode);

  // FIXME we could avoid providing layer, and simply provide the transpose of the survey?
  //surveygraph S{data, layer, method, methodval, mincomps, rsimilarity};
  //S.make_proj();

  SEXP e = PROTECT(Rf_allocVector(VECSXP, 3));
  //cppvector_to_rdf(S.g_agent, c, e);

  UNPROTECT(1);

  return e;
}
