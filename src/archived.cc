#include <R.h>
#include <Rdefines.h>

/*

In this file we archive code snippets, such as functions, that we found helpful
when learning the C interface for R.

*/

SEXP archived_inputoutput(SEXP m, SEXP n) 
{
  SEXP result = PROTECT(NEW_INTEGER(1));
  INTEGER(result)[0] = 2 * INTEGER(m)[0] + 3 * INTEGER(n)[0];
  Rprintf("processing %d and %d\n", INTEGER(m)[0], INTEGER(n)[0]);
  Rprintf("output will be %d\n", INTEGER(result)[0]);
  UNPROTECT(1);
  return result;
}

SEXP archived_hwnumeric(SEXP a, SEXP b)
{
  SEXP result = PROTECT(NEW_NUMERIC(1));
  REAL(result)[0] = REAL(a)[0] + REAL(b)[0];
  Rprintf("hello world! the sum of %f and %f is %f\n", REAL(a)[0], REAL(b)[0], REAL(result)[0]);
  Rprintf("the value of REALSXP is %d\n", REALSXP);
  UNPROTECT(1);
  return result;
}

SEXP archived_hwinteger(SEXP a, SEXP b) 
{
  SEXP result = PROTECT(NEW_INTEGER(1));
  INTEGER(result)[0] = INTEGER(a)[0] + INTEGER(b)[0];
  Rprintf("hello world! the sum of %d and %d is %d\n", INTEGER(a)[0], INTEGER(b)[0], INTEGER(result)[0]);
  Rprintf("the value of INTSXP is %d\n", INTSXP);
  UNPROTECT(1);
  return result;
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
