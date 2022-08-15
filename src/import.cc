#include <R.h>
#include <Rdefines.h>

#include <vector>

static void print_df_r(SEXP x) {
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

static void convert_df_vecvec(SEXP x) {
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
SEXP rsurveygraph_df_check(SEXP x) {
  int len = length(x);
  SEXP result = PROTECT(NEW_CHARACTER(len));

  for(int i = 0; i < len; ++i) {
    switch(TYPEOF(VECTOR_ELT(x, i))) {
      case(REALSXP):
        CHARACTER_POINTER(result)[i] = mkChar("numeric");
        break;
      case(INTSXP):
        CHARACTER_POINTER(result)[i] = mkChar("integer");
        break;
      case(LGLSXP):
        CHARACTER_POINTER(result)[i] = mkChar("logical");
        break;
      case(STRSXP):
        CHARACTER_POINTER(result)[i] = mkChar("character");
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

SEXP rsurveygraph_hw_num(SEXP a, SEXP b) {
  SEXP result = PROTECT(NEW_NUMERIC(1));
  REAL(result)[0] = REAL(a)[0] + REAL(b)[0];
  Rprintf("hello world! the sum of %f and %f is %f\n", REAL(a)[0], REAL(b)[0], REAL(result)[0]);
  Rprintf("the value of REALSXP is %d\n", REALSXP);
  UNPROTECT(1);
  return result;
}

SEXP rsurveygraph_hw_int(SEXP a, SEXP b) {
  SEXP result = PROTECT(NEW_INTEGER(1));
  INTEGER(result)[0] = INTEGER(a)[0] + INTEGER(b)[0];
  Rprintf("hello world! the sum of %d and %d is %d\n", INTEGER(a)[0], INTEGER(b)[0], INTEGER(result)[0]);
  Rprintf("the value of INTSXP is %d\n", INTSXP);
  UNPROTECT(1);
  return result;
}
