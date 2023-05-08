#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP surveygraphr_pilot(SEXP m);
extern SEXP surveygraphr_dfmanip(SEXP m);

extern SEXP archived_inputoutput(SEXP m, SEXP n);
extern SEXP archived_hwinteger(SEXP a, SEXP b);
extern SEXP archived_hwnumeric(SEXP a, SEXP b);
extern SEXP archived_dftypes(SEXP a);
extern SEXP archived_vectormanip(SEXP m);

static const R_CallMethodDef R_CallDef[] = {
  // live routines
  {"surveygraphr_pilot",        (DL_FUNC) &surveygraphr_pilot, 1},
  {"surveygraphr_dfmanip",      (DL_FUNC) &surveygraphr_dfmanip, 1},

  // archived routines
  {"archived_inputoutput",      (DL_FUNC) &archived_inputoutput, 2},
  {"archived_hwinteger",        (DL_FUNC) &archived_hwinteger, 2},
  {"archived_hwnumeric",        (DL_FUNC) &archived_hwnumeric, 2},
  {"archived_dftypes",          (DL_FUNC) &archived_dftypes, 1},
  {"archived_vectormanip",      (DL_FUNC) &archived_vectormanip, 1},

  {NULL, NULL, 0}
};

extern "C" void R_init_surveygraphr(DllInfo *dll) 
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
