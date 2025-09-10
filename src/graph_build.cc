#include "graph.h"
//#include "surveygraph.h"

#include <cmath>

// FIXME we can delete this now, right? this is standalone cpp
// TODO delete this when you no longer need Rprintf
#define R_NO_REMAP
#include "R.h"
#include "Rdefines.h"

using namespace std;


// build graph using Euclidean distance or cosine similarity
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

      //if(w >= threshold){
      if(w > threshold - 1e-9){
        //Rprintf("weight of %d %d is %f\n", i, j, w);
        network[i].insert(neighbour{int(j), w});
        network[j].insert(neighbour{int(i), w});
        avg_degree += 2;
        e += 1;
      }
    }
  }

  // use normalised average degree
  avg_degree /= double(S.size());
}

// Manhattan distance between rows or columns u and v
void graph::dist_manhattan(const survey &S, const int &u, const int &v, double &w)
{
  int count = 0;  // enumerates pairs of entries that aren't NaN
  w = 0;
  for(int j = 0; j < S[0].size(); ++j){
    //Rprintf("%d %d %f %f is comparison\n", u, v, S[u][i], S[v][i]);
    //Rprintf("%d %f %f %d %d\n", j, S[u][j], S[v][j], !isnan(S[u][j]), !isnan(S[v][j]));
    if(!isnan(S[u][j]) && !isnan(S[v][j])){
      w += abs(S[u][j] - S[v][j]);
      ++count;
    }
  }

  // normalise by the number of valid comparisons
  if(count > 0)
    w = 1.0 - w / double(count);

  //Rprintf("%d %f %d count weight mincomps\n", count, w, mincomps);

  // If the number of valid comparisons is less than a specified threshold,
  // set to -1, and this edge will not be included in edge list.
  if(count < mincomps)
    w = -1;
}

// Manhattan distance between rows or columns u and v
void graph::dist_euclidean(const survey &S, const int &u, const int &v, double &w)
{
  int count = 0;  // enumerates pairs of entries that aren't NaN
  w = 0;
  for(int j = 0; j < S[0].size(); ++j){
    //Rprintf("%d %d %f %f is comparison\n", u, v, S[u][i], S[v][i]);
    if(!isnan(S[u][j]) && !isnan(S[v][j])){
      w += (S[u][j] - S[v][j]) * (S[u][j] - S[v][j]);
      ++count;
    }
  }

  // normalise by the number of valid comparisons
  if(count > 0)
    w = 1.0 - w / double(count);
  w = sqrt(w);

  // If the number of valid comparisons is less than a specified threshold,
  // set to -1, and this edge will not be included in edge list.
  if(count < mincomps)
    w = -1;
}

//// Manhattan distance between rows or columns u and v
//// comparisoncnt counts comparisons of entries that aren't NaNs.
//void graph::dist_manhattan(const surveydef &S, const int &u, const int &v, double &w)
//{
//  switch(layer){
//    int comparisoncnt;
//    case Layer::agent:
//      w = 0;
//      comparisoncnt = 0;  
//      for(int i = 0; i < m; ++i){
//        //Rprintf("%d %d %f %f is comparison\n", u, v, S[u][i], S[v][i]);
//        if(!isnan(S[u][i]) && !isnan(S[v][i])){
//          w += abs(S[u][i] - S[v][i]);
//          ++comparisoncnt;
//        }
//      }
//
//      // normalise by the number of valid comparisons
//      if(comparisoncnt > 0)
//        w = 1.0 - w / double(comparisoncnt);
//
//      // If the number of valid comparisons is less than a specified threshold,
//      // set to -1, and this edge will not be included in edge list.
//      if(comparisoncnt < mincomps)
//        w = -1;
//
//      break;
//    case Layer::symbolic:
//      w = 0;
//      comparisoncnt = 0;
//      // TODO FIXME shouldn't this be n? why isn't it?
//      for(int i = 0; i < m; ++i){
//        if(!isnan(S[u][i]) && !isnan(S[v][i])){
//          w += abs(S[i][u] - S[i][v]);
//          ++comparisoncnt;
//        }
//      }
//
//      if(comparisoncnt > 0)
//        w = 1.0 - w / double(comparisoncnt);
//
//      if(comparisoncnt < mincomps)
//        w = -1;
//
//      break;
//  }
//}
