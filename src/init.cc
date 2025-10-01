#include <stdlib.h>          // NULL

//#ifndef R_NO_REMAP
//#define R_NO_REMAP
//#endif
#include <Rinternals.h>      // SEXP
#include <R_ext/Rdynload.h>  // R_CallMethodDef, DllInfo etc


extern SEXP rmake_projection(
  SEXP rdata,         // dataframe containing survey
  SEXP rlayer,        // layer to project on
  SEXP rmethod,       // sparsification method
  SEXP rmethodval,    // method value
  SEXP rcomparisons,  // minimum comparisons
  SEXP rmetric,       // similarity metric
  SEXP rbootreps,     // number of bootstrap realisations
  SEXP rbootval,      // utility variable for boostrapping
  SEXP rbootseed      // boostrapping seed
);

extern SEXP rmake_threshold_profile(
  SEXP rdata,         // dataframe containing survey
  SEXP rlayer,        // layer to project on
  SEXP rcomparisons,  // minimum comparisons
  SEXP rmetric,       // similarity metric
  SEXP rcount         // similarity count in profile
);

static const R_CallMethodDef R_CallDef[] = {
  {"rmake_projection",             (DL_FUNC) &rmake_projection, 9},
  {"rmake_threshold_profile",      (DL_FUNC) &rmake_threshold_profile, 5},

  {NULL, NULL, 0}
};

extern "C" void R_init_surveygraph(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
