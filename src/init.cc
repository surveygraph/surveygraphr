#include <stdlib.h>

#define R_NO_REMAP
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP rmake_projection(
  SEXP rdata,        // dataframe containing survey
  SEXP rlayer,       // layer to project on
  SEXP rmethod,      // sparsification method
  SEXP rmethodval,   // method value
  SEXP rmincompare,  // minimum comparisons
  SEXP rmetric       // similarity metric
);

//extern SEXP rmake_projection(
//  SEXP rdata,       // dataframe
//  SEXP rlayer,      // layer flag
//  SEXP rmethod,     // sparsification method
//  SEXP rmethodval,  // method value
//  SEXP rdummycode,  // dummy coding flag
//  SEXP rlikert,     // likert scale specification
//  SEXP rmincomps,   // minimum comparisons
//  SEXP rsimilarity, // similarity metric
//  SEXP rcentre      // centering flag
//);

extern SEXP rmake_proj_agent_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_agent_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_agent_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_symbolic_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_symbolic_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_symbolic_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_threshold_profile_agent(SEXP m);
extern SEXP rmake_threshold_profile_symbolic(SEXP m);

static const R_CallMethodDef R_CallDef[] = {
  // R package entry points
  //{"rmake_projection",             (DL_FUNC) &rmake_projection, 9},
  {"rmake_projection",             (DL_FUNC) &rmake_projection, 6},
  {"rmake_proj_agent_lcc",         (DL_FUNC) &rmake_proj_agent_lcc, 4},
  {"rmake_proj_agent_ad",          (DL_FUNC) &rmake_proj_agent_ad, 4},
  {"rmake_proj_agent_similar",     (DL_FUNC) &rmake_proj_agent_similar, 4},

  {"rmake_proj_symbolic_lcc",      (DL_FUNC) &rmake_proj_symbolic_lcc, 4},
  {"rmake_proj_symbolic_ad",       (DL_FUNC) &rmake_proj_symbolic_ad, 4},
  {"rmake_proj_symbolic_similar",  (DL_FUNC) &rmake_proj_symbolic_similar, 4},

  {"rmake_threshold_profile_agent",      (DL_FUNC) &rmake_threshold_profile_agent, 1},
  {"rmake_threshold_profile_symbolic",   (DL_FUNC) &rmake_threshold_profile_symbolic, 1},

  {NULL, NULL, 0}
};

extern "C" void R_init_surveygraph(DllInfo *dll) 
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
