#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP rsurveygraph_hw_int(SEXP a, SEXP b);
extern SEXP rsurveygraph_hw_num(SEXP a, SEXP b);
extern SEXP rsurveygraph_df_check(SEXP a);
//extern SEXP rsurveygraph_add(SEXP a, SEXP b);
//extern SEXP rsurveygraph_add(SEXP a, SEXP b);
//extern SEXP rsurveygraph_add(SEXP a, SEXP b);
//extern SEXP rsurveygraph_add(SEXP a, SEXP b);
//extern SEXP rsurveygraph_add(SEXP a, SEXP b);

static const R_CallMethodDef R_CallDef[] = {
  {"rsurveygraph_df_check",  (DL_FUNC) &rsurveygraph_df_check, 1},
  {"rsurveygraph_hw_int",    (DL_FUNC) &rsurveygraph_hw_int, 2},
  {"rsurveygraph_hw_num",    (DL_FUNC) &rsurveygraph_hw_num, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},

  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},

  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  //{"rsurveygraph_add", (DL_FUNC) &rsurveygraph_add, 2},
  {NULL, NULL, 0}
};

extern "C" void R_init_rsurveygraph(DllInfo *dll) {
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
