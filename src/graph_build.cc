#include "surveygraph.h"

#define R_NO_REMAP
#include "R.h"
#include "Rdefines.h"

using namespace std;

// build graph using Euclidean distance or cosine similarity
void graph::build_graph(const surveydef &S)
{
  network = std::map<int, std::set<neighbour>>{};
  for(int i = 0; i < n; ++i) network[i] = std::set<neighbour>{};

  e = 0;
  avg_degree = 0;
  for(int i = 0; i < n; ++i){
    for(int j = i + 1; j < n; ++j){
      double w = 0.0;
      man_distance(S, int(i), int(j), w);
      if(w > threshold){
        network[i].insert(neighbour{int(j), w});
        network[j].insert(neighbour{int(i), w});
        avg_degree += 2;
        e += 1;
      }
    }
  }

  // use normalised average degree
  avg_degree /= double(n);
}

// Manhattan distance between rows or columns u and v
// comparisoncnt counts comparisons of entries that aren't NaNs.
void graph::man_distance(const surveydef &S, const int &u, const int &v, double &w)
{
  switch(layer){
    int comparisoncnt;
    case Layer::agent:
      w = 0;
      comparisoncnt = 0;  
      for(int i = 0; i < m; ++i){
        //Rprintf("%d %d %f %f is comparison\n", u, v, S[u][i], S[v][i]);
        if(!isnan(S[u][i]) && !isnan(S[v][i])){
          w += abs(S[u][i] - S[v][i]);
          ++comparisoncnt;
        }
      }

      // normalise by the number of valid comparisons
      if(comparisoncnt > 0)
        w = 1.0 - w / double(comparisoncnt);

      // If the number of valid comparisons is less than a specified threshold,
      // set to -1, and this edge will not be included in edge list.
      if(comparisoncnt < mincomps)
        w = -1;

      break;
    case Layer::symbolic:
      w = 0;
      comparisoncnt = 0;
      // TODO FIXME shouldn't this be n? why isn't it?
      for(int i = 0; i < m; ++i){
        if(!isnan(S[u][i]) && !isnan(S[v][i])){
          w += abs(S[i][u] - S[i][v]);
          ++comparisoncnt;
        }
      }

      if(comparisoncnt > 0)
        w = 1.0 - w / double(comparisoncnt);

      if(comparisoncnt < mincomps)
        w = -1;

      break;
  }
}
