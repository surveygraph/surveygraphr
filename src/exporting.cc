#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

// read in a data frame and output a different data frame, assuming double
SEXP surveygraphr_buildresp(SEXP df) 
{
  int n = length(df);
  int m = length(VECTOR_ELT(df, 0));

  // read a dataframe into a vector of vectors
  std::vector<std::vector<double>> survey(m, std::vector<double> (n));
  SEXP dummy = PROTECT(allocVector(REALSXP, m));
  for(int j = 0; j < n; ++j) {
    dummy = VECTOR_ELT(df, j);
    for(int i = 0; i < m; ++i) {
      survey[i][j] = REAL(dummy)[i];
    }
  }

  // build the corresponding network
  surveygraph S;
  S.m = m;
  S.n = n;
  S.surveyvec = survey;
  S.buildrespondentgraph();

  int ecount = 0;
  for(auto &it : S.G) ecount += it.second.size();
  if(ecount % 2 == 0) {
    ecount /= 2;
  } else {
    Rprintf("you don't have an even number of edges for some reason\n");
  }

  SEXP edgelist = PROTECT(allocVector(INTSXP, 2 * ecount));

  int i = 0;
  for(auto &it : S.G) {
    for(auto &jt : it.second) {
      if(it.first < jt.u) {
        INTEGER(edgelist)[i] = it.first + 1;
        INTEGER(edgelist)[i + 1] = jt.u + 1;
        i += 2;
      }
    }
  }

  UNPROTECT(2);
  return edgelist;
}

static void print_df_r(SEXP x) 
{
  int m = length(x);
  for(int i = 0; i < m; ++i) {
    printf("%d : ", i);
    SEXP dummy = PROTECT(VECTOR_ELT(x, i));
    int n = length(dummy);
    for(int j = 0; j < n; ++j) {
      printf("%f ", REAL_ELT(dummy, j));
    }
    UNPROTECT(1);
    printf("\n");
  }
}

static void convert_df_vecvec(SEXP x) 
{
  int m = length(x);
  std::vector<std::vector<int>> s(m);
  for(int i = 0; i < m; ++i) {
    printf("%d : ", i);
    SEXP dummy = PROTECT(VECTOR_ELT(x, i));
    int n = length(dummy);
    for(int j = 0; j < n; ++j) {
      printf("%f ", REAL_ELT(dummy, j));
    }
    UNPROTECT(1);
    printf("\n");
  }
}

// checks data type of each dataframe column
SEXP surveygraphr_df_check(SEXP x) 
{
  int len = length(x);
  SEXP result = PROTECT(NEW_CHARACTER(len));

  for(int i = 0; i < len; ++i) {
    switch(TYPEOF(VECTOR_ELT(x, i))) {
      case(REALSXP):
        CHARACTER_POINTER(result)[i] = mkChar("numeric");
        Rprintf("%f\n", VECTOR_ELT(x, i));
        break;
      case(INTSXP):
        CHARACTER_POINTER(result)[i] = mkChar("integer");
        Rprintf("%d\n", VECTOR_ELT(x, i));
        break;
      case(LGLSXP):
        CHARACTER_POINTER(result)[i] = mkChar("logical");
        Rprintf("%d\n", VECTOR_ELT(x, i));
        break;
      case(STRSXP):
        CHARACTER_POINTER(result)[i] = mkChar("character");
        Rprintf("%c\n", VECTOR_ELT(x, i));
        break;
      case(VECSXP):
        CHARACTER_POINTER(result)[i] = mkChar("list");
        break;
      default:
        CHARACTER_POINTER(result)[i] = mkChar("dunnoooo");
    }
  }
  UNPROTECT(1);
  return result;
}

// read in a numeric vector, output a modified vector
SEXP surveygraphr_vecmanip(SEXP x) 
{
  int len = length(x);
  SEXP result = PROTECT(NEW_NUMERIC(len)); // allocVector(REALSXP, len)
  if(TYPEOF(x) != REALSXP) Rprintf("you're fucked, not a double\n");
  for(int i = 0; i < len; ++i) {
    REAL(result)[i] = 10 * REAL(x)[i] + 1;
  }
  UNPROTECT(1);
  return result;
}

// read in a data frame and output a different data frame, assuming double
SEXP surveygraphr_dfmanip(SEXP x) 
{
  SEXP result = PROTECT(allocVector(VECSXP, 2));

  SEXP oldcol1 = PROTECT(allocVector(REALSXP, 3));
  SEXP oldcol2 = PROTECT(allocVector(REALSXP, 3));

  SEXP newcol1 = PROTECT(allocVector(REALSXP, 6));
  SEXP newcol2 = PROTECT(allocVector(REALSXP, 6));

  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SEXP rownames = PROTECT(allocVector(INTSXP, 2));

  SET_STRING_ELT(names, 0, mkChar("x")); // name first column x
  SET_STRING_ELT(names, 1, mkChar("y")); // name second column y

  INTEGER(rownames)[0] = NA_INTEGER;  // default entry if size below too small
  INTEGER(rownames)[1] = -6;          // number of rows

  oldcol1 = VECTOR_ELT(x, 0);   // extract first column from x
  oldcol2 = VECTOR_ELT(x, 1);   // extract second column from x

  // set elements of data frame
  REAL(newcol1)[0] = REAL(oldcol1)[0];
  REAL(newcol1)[1] = REAL(oldcol1)[1];
  REAL(newcol1)[2] = REAL(oldcol1)[2];
  REAL(newcol1)[3] = 4.0;
  REAL(newcol1)[4] = 5.0;
  REAL(newcol1)[5] = 6.0;
  REAL(newcol2)[0] = REAL(oldcol2)[0];
  REAL(newcol2)[1] = REAL(oldcol2)[1];
  REAL(newcol2)[2] = REAL(oldcol2)[2];
  REAL(newcol2)[3] = 8.0;
  REAL(newcol2)[4] = 10.0;
  REAL(newcol2)[5] = 12.0;

  SET_VECTOR_ELT(result, 0, newcol1);
  SET_VECTOR_ELT(result, 1, newcol2);

  setAttrib(result, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(result, R_RowNamesSymbol, rownames);
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(7);
  return result;
}

// practice function, temp
SEXP surveygraphr_hw_num(SEXP a, SEXP b)
{
  SEXP result = PROTECT(NEW_NUMERIC(1));
  REAL(result)[0] = REAL(a)[0] + REAL(b)[0];
  Rprintf("hello world! the sum of %f and %f is %f\n", REAL(a)[0], REAL(b)[0], REAL(result)[0]);
  Rprintf("the value of REALSXP is %d\n", REALSXP);
  UNPROTECT(1);
  return result;
}

// practice function, temp
SEXP surveygraphr_hw_int(SEXP a, SEXP b) 
{
  SEXP result = PROTECT(NEW_INTEGER(1));
  INTEGER(result)[0] = INTEGER(a)[0] + INTEGER(b)[0];
  Rprintf("hello world! the sum of %d and %d is %d\n", INTEGER(a)[0], INTEGER(b)[0], INTEGER(result)[0]);
  Rprintf("the value of INTSXP is %d\n", INTSXP);
  UNPROTECT(1);
  return result;
}
