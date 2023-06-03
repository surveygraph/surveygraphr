#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

static void df_to_cppvector(const SEXP &df, std::vector<vector<double>> &stmp)
{
  int ncol = length(df); // should be column first, right?
  int nrow = length(VECTOR_ELT(df, 0));

  stmp = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));

  SEXP dummy = PROTECT(allocVector(REALSXP, nrow));
  for(int j = 0; j < ncol; ++j){
    dummy = VECTOR_ELT(df, j);
    for(int i = 0; i < nrow; ++i){
      stmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporarily assumes 1 to 10
    }
  }
}

static void vectors_to_df(map<int, set<neighbour>> &g, SEXP &c, SEXP &df)
{
  int dummysize = 0;
  for(auto &it : g){
    for(auto &jt : it.second){
      if(it.first < jt.u) dummysize += 1;
    }
  }
  SEXP u_vector = PROTECT(allocVector(INTSXP, dummysize));  // u column
  SEXP v_vector = PROTECT(allocVector(INTSXP, dummysize));  // v column
  SEXP w_vector = PROTECT(allocVector(REALSXP, dummysize)); // weight column

  int i = 0;
  for(auto &it : g){
    for(auto &jt : it.second){
      if(it.first < jt.u){
        //Rprintf("%d %d %f\n", it.first + 1, jt.u + 1, jt.w);
        INTEGER(u_vector)[i] = it.first + 1;
        INTEGER(v_vector)[i] = jt.u + 1;
        REAL(w_vector)[i] = int(100.0 * jt.w) / 100.0;
        if(INTEGER(c)[0] == 0) REAL(w_vector)[i] += 1.0;
        i += 1;
      }
    }
  }

  SET_VECTOR_ELT(df, 0, u_vector);
  SET_VECTOR_ELT(df, 1, v_vector);
  SET_VECTOR_ELT(df, 2, w_vector);

  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("u"));            // name first column u
  SET_STRING_ELT(names, 1, mkChar("v"));            // name second column v
  SET_STRING_ELT(names, 2, mkChar("weight"));       // name third column weight, required by igraph

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(u_vector);         // number of rows in agent edge list

  setAttrib(df, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(df, R_RowNamesSymbol, rownames);
  setAttrib(df, R_NamesSymbol, names);

  UNPROTECT(5);
}

SEXP rmake_proj_agent_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);

  surveygraph S{surveytmp, 0, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_lcc();

  //Rprintf("hello from... %d\n", S.g_agent.size());

  //for(int i = 0; i < 10; ++i){
  //  Rprintf("%d : ", i);
  //  for(auto jt : S.g_agent[i]){
  //    Rprintf("%d ", jt.u);
  //  }
  //  Rprintf("\n");
  //}

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(2);

  return e;
}

SEXP rmake_proj_agent_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);

  surveygraph S{surveytmp, 1, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_ad();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(2);

  return e;
}

SEXP rmake_proj_agent_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);

  surveygraph S{surveytmp, 2, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_similar();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(2);

  return e;
}

SEXP rmake_proj_symbolic_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);

  surveygraph S{surveytmp, 0, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_lcc();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(2);

  return e;
}

SEXP rmake_proj_symbolic_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);

  surveygraph S{surveytmp, 1, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_ad();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(2);

  return e;
}

SEXP rmake_proj_symbolic_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);

  surveygraph S{surveytmp, 2, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_similar();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(2);

  return e;
}
