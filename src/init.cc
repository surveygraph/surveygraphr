#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP surveygraphr_make_projection(SEXP m, SEXP rlcc, SEXP ilcc);
extern SEXP surveygraphr_make_projection_agent(SEXP m, SEXP rlcc);
extern SEXP surveygraphr_make_projection_symbolic(SEXP m, SEXP ilcc);
extern SEXP surveygraphr_sweep_thresholds(SEXP m);

extern SEXP archived_inputoutput(SEXP m, SEXP n);
extern SEXP archived_hwinteger(SEXP a, SEXP b);
extern SEXP archived_hwnumeric(SEXP a, SEXP b);
extern SEXP archived_dftypes(SEXP a);
extern SEXP archived_vectormanip(SEXP m);
extern SEXP archived_dfmanip(SEXP m);

static const R_CallMethodDef R_CallDef[] = {
  // R package entry points
  {"surveygraphr_make_projection",           (DL_FUNC) &surveygraphr_make_projection, 3},
  {"surveygraphr_make_projection_agent",     (DL_FUNC) &surveygraphr_make_projection_agent, 2},
  {"surveygraphr_make_projection_symbolic",  (DL_FUNC) &surveygraphr_make_projection_symbolic, 2},
  {"surveygraphr_sweep_thresholds",          (DL_FUNC) &surveygraphr_sweep_thresholds, 1},

  // archived routines, could call but we don't
  {"archived_inputoutput",      (DL_FUNC) &archived_inputoutput, 2},
  {"archived_hwinteger",        (DL_FUNC) &archived_hwinteger, 2},
  {"archived_hwnumeric",        (DL_FUNC) &archived_hwnumeric, 2},
  {"archived_dftypes",          (DL_FUNC) &archived_dftypes, 1},
  {"archived_vectormanip",      (DL_FUNC) &archived_vectormanip, 1},
  {"archived_dfmanip",          (DL_FUNC) &archived_dfmanip, 1},

  {NULL, NULL, 0}
};

extern "C" void R_init_surveygraphr(DllInfo *dll) 
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
