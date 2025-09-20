#include "graph.h"

#include <cmath>  // sqrt, abs

#define R_NO_REMAP     // TODO comment out for CRAN, only used for debugging
#include <R.h>         // with Rprint.
#include <Rdefines.h>  //

using namespace std;


void graph::build_graph(const survey &S)
{
  network = std::map<int, std::set<neighbour>>{};
  for(int i = 0; i < S.size(); ++i) network[i] = std::set<neighbour>{};

  e = 0;
  avg_degree = 0;
  for(int i = 0; i < S.size(); ++i){
    for(int j = i + 1; j < S.size(); ++j){
      double w = 0.0;
      if(metric == 0)
        dist_manhattan(S, int(i), int(j), w);
      else if(metric == 1)
        dist_euclidean(S, int(i), int(j), w);

      // TODO make a decision on this
      //if(w >= threshold){
      if(w > threshold - 1e-9 && w != -1){
        network[i].insert(neighbour{int(j), w});
        network[j].insert(neighbour{int(i), w});
        avg_degree += 2.0;
        e += 1;
      }
    }
  }

  // Use normalised average degree.
  avg_degree /= double(S.size());
}


void graph::build_complete(const survey &S)
{
  edgelist = std::set<edge>{};

  for(int i = 0; i < S.size(); ++i){
    for(int j = i + 1; j < S.size(); ++j){
      double w = 0.0;
      if(metric == 0)
        dist_manhattan(S, int(i), int(j), w);
      else if(metric == 1)
        dist_euclidean(S, int(i), int(j), w);

        edgelist.insert(edge{set<int>{i, j}, w});
    }
  }
}


// Manhattan distance between rows or columns u and v
void graph::dist_manhattan(const survey &S, const int &u, const int &v, double &w)
{
  // Enumerates pairs of entries in which neither are NaN.
  int count = 0;  

  w = 0;
  for(int j = 0; j < S[0].size(); ++j){
    if(!isnan(S[u][j]) && !isnan(S[v][j])){
      w += abs(S[u][j] - S[v][j]);
      ++count;
    }
  }

  // Normalise by the number of valid comparisons.
  if(count > 0)
    w = 1.0 - w / double(count);

  // If the number of valid comparisons is less than a specified threshold,
  // set to -1, and this edge will not be included in edge list.
  if(count < mincomps)
    w = -1;
}

// Manhattan distance between rows or columns u and v
void graph::dist_euclidean(const survey &S, const int &u, const int &v, double &w)
{
  // Enumerates pairs of entries in which neither are NaN.
  int count = 0;  

  w = 0;
  for(int j = 0; j < S[0].size(); ++j){
    if(!isnan(S[u][j]) && !isnan(S[v][j])){
      w += (S[u][j] - S[v][j]) * (S[u][j] - S[v][j]);
      ++count;
    }
  }

  // Normalise by the number of valid comparisons.
  if(count > 0)
    w = w / double(count);

  w = 1 - sqrt(w);

  // If the number of valid comparisons is less than a specified threshold,
  // set to -1, and this edge will not be included in edge list.
  if(count < mincomps)
    w = -1;
}
