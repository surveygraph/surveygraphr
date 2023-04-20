#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP surveygraphr_buildresp(SEXP m);
extern SEXP surveygraphr_dfmanip(SEXP m);
extern SEXP surveygraphr_vecmanip(SEXP m);
extern SEXP surveygraphr_inputdf(SEXP m);
extern SEXP surveygraphr_dummy(SEXP m, SEXP n);
extern SEXP surveygraphr_hw_int(SEXP a, SEXP b);
extern SEXP surveygraphr_hw_num(SEXP a, SEXP b);
extern SEXP surveygraphr_df_check(SEXP a);
//extern SEXP surveygraphr_writeg(void);

static const R_CallMethodDef R_CallDef[] = {
  {"surveygraphr_buildresp",    (DL_FUNC) &surveygraphr_buildresp, 1},
  {"surveygraphr_dfmanip",      (DL_FUNC) &surveygraphr_dfmanip, 1},
  {"surveygraphr_vecmanip",     (DL_FUNC) &surveygraphr_vecmanip, 1},
  {"surveygraphr_inputdf",      (DL_FUNC) &surveygraphr_inputdf, 1},
  {"surveygraphr_dummy",        (DL_FUNC) &surveygraphr_dummy, 2},
  {"surveygraphr_hw_int",       (DL_FUNC) &surveygraphr_hw_int, 2},
  {"surveygraphr_hw_num",       (DL_FUNC) &surveygraphr_hw_num, 2},
  {"surveygraphr_df_check",     (DL_FUNC) &surveygraphr_df_check, 1},
  {NULL, NULL, 0}
};

extern "C" void R_init_surveygraphr(DllInfo *dll) 
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
