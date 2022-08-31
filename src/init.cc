#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP surveygraphr_hw_int(SEXP a, SEXP b);
extern SEXP surveygraphr_hw_num(SEXP a, SEXP b);
extern SEXP surveygraphr_df_check(SEXP a);
//extern SEXP surveygraphr_writeg(void);

//extern SEXP surveygraphr_add(SEXP a, SEXP b);
//extern SEXP surveygraphr_add(SEXP a, SEXP b);
//extern SEXP surveygraphr_add(SEXP a, SEXP b);
//extern SEXP surveygraphr_add(SEXP a, SEXP b);
//extern SEXP surveygraphr_add(SEXP a, SEXP b);

static const R_CallMethodDef R_CallDef[] = {
  {"surveygraphr_hw_int",    (DL_FUNC) &surveygraphr_hw_int, 2},
  {"surveygraphr_hw_num",    (DL_FUNC) &surveygraphr_hw_num, 2},
  {"surveygraphr_df_check",  (DL_FUNC) &surveygraphr_df_check, 1},
  //{"surveygraphr_writeg",    (DL_FUNC) &surveygraphr_writeg, 0},

  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},

  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},

  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  //{"surveygraphr_add", (DL_FUNC) &surveygraphr_add, 2},
  {NULL, NULL, 0}
};

extern "C" void R_init_surveygraphr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
