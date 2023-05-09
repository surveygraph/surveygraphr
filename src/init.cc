#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP surveygraphr_list_graphs(SEXP m);
extern SEXP surveygraphr_explore_graphs(SEXP m);

extern SEXP archived_inputoutput(SEXP m, SEXP n);
extern SEXP archived_hwinteger(SEXP a, SEXP b);
extern SEXP archived_hwnumeric(SEXP a, SEXP b);
extern SEXP archived_dftypes(SEXP a);
extern SEXP archived_vectormanip(SEXP m);
extern SEXP archived_dfmanip(SEXP m);

static const R_CallMethodDef R_CallDef[] = {
  // R package entry points
  {"surveygraphr_list_graphs",     (DL_FUNC) &surveygraphr_list_graphs, 1},
  {"surveygraphr_explore_graphs",  (DL_FUNC) &surveygraphr_explore_graphs, 1},

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
