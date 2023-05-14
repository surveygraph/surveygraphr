#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

// read in a data frame and output list containing two integer vectors
// lists, containing edge lists for respondent and item graphs
SEXP surveygraphr_graph_edgelists(SEXP df, SEXP rlcc, SEXP ilcc) 
{
  int n = length(df); // should be column first, right?
  int m = length(VECTOR_ELT(df, 0));

  // read a dataframe into a vector of vectors
  std::vector<std::vector<double>> surveytmp(m, std::vector<double> (n));
  SEXP dummy = PROTECT(allocVector(REALSXP, m));
  for(int j = 0; j < n; ++j) {
    dummy = VECTOR_ELT(df, j);
    for(int i = 0; i < m; ++i) {
      //surveytmp[i][j] = (REAL(dummy)[i] - 3) / 2;     // temporary, assumes 1 to 5
      surveytmp[i][j] = (REAL(dummy)[i] - 5.5) / 4.5; // temporary, assumes 1 to 10
    }
  }

  double rlcctmp = REAL(rlcc)[0];
  double ilcctmp = REAL(ilcc)[0];
  surveygraph S{surveytmp, rlcctmp, ilcctmp}; // constructor for surveygraph class
  S.graph_edgelists_pilot();

  // count edges in respondent graph
  int ecount = 0;
  for(auto &it : S.g_respondents) ecount += it.second.size();
  if(ecount % 2 == 0) {
    ecount /= 2;
  } else {
    Rprintf("ERROR: respondent graph has uneven number of edges\n");
  }
  SEXP e_respondents = PROTECT(allocVector(INTSXP, 2 * ecount));

  // count edges in item graph
  ecount = 0;
  for(auto &it : S.g_items) ecount += it.second.size();
  if(ecount % 2 == 0) {
    ecount /= 2;
  } else {
    Rprintf("ERROR: item graph has uneven number of edges\n");
  }
  SEXP e_items = PROTECT(allocVector(INTSXP, 2 * ecount));

  // create integer vector for respondent graph edge list
  int i = 0;
  for(auto &it : S.g_respondents) {
    for(auto &jt : it.second) {
      if(it.first < jt.u) {
        INTEGER(e_respondents)[i] = it.first + 1;
        INTEGER(e_respondents)[i + 1] = jt.u + 1;
        i += 2;
      }
    }
  }

  // create integer vector for item graph edge list
  i = 0;
  for(auto &it : S.g_items) {
    for(auto &jt : it.second) {
      if(it.first < jt.u) {
        INTEGER(e_items)[i] = it.first + 1;
        INTEGER(e_items)[i + 1] = jt.u + 1;
        i += 2;
      }
    }
  }

  // return a list containing the two edge sets
  //SEXP edgelists = PROTECT(allocVector(VECSXP, 1));
  //SET_VECTOR_ELT(edgelists, 0, e_respondents);
  //SET_VECTOR_ELT(edgelists, 1, e_items);

  UNPROTECT(3);
  //UNPROTECT(4);

  return e_respondents;
  //return edgelists;
}
