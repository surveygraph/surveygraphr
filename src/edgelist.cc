#include "surveygraph.h"

#include <cmath>  // sqrt, abs

#define R_NO_REMAP     // TODO comment out for CRAN, only used for debugging
#include <R.h>         // with Rprint.
#include <Rdefines.h>  //

//using namespace std;


// TODO simpler version of this, simply keep edges in `edgelist` with weights
// greater than or equal to threshold.
void surveygraph::edgelist_thresholded(const double &threshold)
{
  edgelist = std::set<edge>{};
  for(int i = 0; i < survey.size(); ++i){
    for(int j = i + 1; j < survey.size(); ++j){
      double w = 0.0;
      if(metric == 0)
        dist_manhattan(int(i), int(j), w);
      else if(metric == 1)
        dist_euclidean(int(i), int(j), w);

      // TODO make a decision on this
      //if(w >= threshold){
      if(w > threshold - 1e-9 && w != -1){
        edgelist.insert(edge{std::set<int>{i, j}, w});
      }
    }
  }
}


void surveygraph::edgelist_complete()
{
  edgelist = std::set<edge>{};

  for(int i = 0; i < survey.size(); ++i){
    for(int j = i + 1; j < survey.size(); ++j){
      double w = 0.0;
      if(metric == 0)
        dist_manhattan(int(i), int(j), w);
      else if(metric == 1)
        dist_euclidean(int(i), int(j), w);

        edgelist.insert(edge{std::set<int>{i, j}, w});
    }
  }
}


// Manhattan distance between rows or columns u and v
void surveygraph::dist_manhattan(const int &u, const int &v, double &w)
{
  // Enumerates pairs of entries in which neither are NaN.
  int count = 0;  

  w = 0;
  for(int j = 0; j < survey[0].size(); ++j){
    if(!isnan(survey[u][j]) && !isnan(survey[v][j])){
      w += abs(survey[u][j] - survey[v][j]);
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
void surveygraph::dist_euclidean(const int &u, const int &v, double &w)
{
  int count = 0;  

  w = 0;
  for(int j = 0; j < survey[0].size(); ++j){
    if(!isnan(survey[u][j]) && !isnan(survey[v][j])){
      w += (survey[u][j] - survey[v][j]) * (survey[u][j] - survey[v][j]);
      ++count;
    }
  }

  if(count > 0)
    w = w / double(count);

  w = 1 - sqrt(w);

  if(count < mincomps)
    w = -1;
}
