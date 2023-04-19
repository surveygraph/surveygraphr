#include <R.h>
#include <Rdefines.h>

#include <vector>

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

// read in a data frame and output the sum of its elements, assuming double
SEXP surveygraphr_dfmanip(SEXP x) 
{
  int len = 3;
  SEXP result = PROTECT(NEW_LIST(2)); // allocVector(REALSXP, len)

  SEXP col1 = PROTECT(NEW_NUMERIC(3));
  SEXP col2 = PROTECT(NEW_NUMERIC(3));

  SEXP newcol1 = PROTECT(NEW_NUMERIC(3));
  SEXP newcol2 = PROTECT(NEW_NUMERIC(3));

  newcol1 = VECTOR_ELT(x, 0);
  newcol2 = VECTOR_ELT(x, 1);

  REAL(col1)[0] = 1.0;
  REAL(col1)[1] = 2.0;
  REAL(col1)[2] = 3.0;

  REAL(col2)[0] = 2.0;
  REAL(col2)[1] = 4.0;
  REAL(col2)[2] = 6.0;

  //SET_VECTOR_ELT(result, 0, col1);
  //SET_VECTOR_ELT(result, 1, col2);
  SET_VECTOR_ELT(result, 0, newcol1);
  SET_VECTOR_ELT(result, 1, newcol2);

  UNPROTECT(5);
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
