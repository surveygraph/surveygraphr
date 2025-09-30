#include "surveygraph.h"

#include <cmath>  // sqrt, fabs FIXME abs() can cast to integer depending on system!

#define R_NO_REMAP     // TODO comment out for CRAN, only used for debugging
#include <R.h>         // with Rprint.
#include <Rdefines.h>  //


// TODO simpler version of this, simply keep edges in `edgelist` with weights
// greater than or equal to threshold.
void surveygraph::edgelist_thresholded(const double &threshold)
{
  edgelist = std::set<edge>{};
  for(unsigned int i = 0; i < survey.size(); ++i){
    for(unsigned int j = i + 1; j < survey.size(); ++j){
      double w = 0.0;
      if(metric == 0)
        dist_manhattan(int(i), int(j), w);
      else if(metric == 1)
        dist_euclidean(int(i), int(j), w);

      // TODO make a decision on this
      //if(w >= threshold){
      if(w > threshold - 1e-9 && w != -1){
        edgelist.insert(edge{std::set<int>{int(i), int(j)}, w});
      }
    }
  }
}


void surveygraph::edgelist_complete()
{
  edgelist = std::set<edge>{};

  for(unsigned int i = 0; i < survey.size(); ++i){
    for(unsigned int j = i + 1; j < survey.size(); ++j){
      double w = 0.0;
      if(metric == 0)
        dist_manhattan(int(i), int(j), w);
      else if(metric == 1)
        dist_euclidean(int(i), int(j), w);

      	//Rprintf("you're inserting weight %zu %zu %f\n", i, j, w);
        edgelist.insert(edge{std::set<int>{int(i), int(j)}, w});
    }
  }
}


// Manhattan distance between rows or columns u and v
void surveygraph::dist_manhattan(const int &u, const int &v, double &w)
{
  // Enumerates pairs of entries in which neither are NaN.
  int count = 0;  

  w = 0;
  for(unsigned int j = 0; j < survey[0].size(); ++j){
    if(!std::isnan(survey[u][j]) && !std::isnan(survey[v][j])){
	    //Rprintf("w before is %f\n", w);
      w += fabs(survey[u][j] - survey[v][j]);
	    //Rprintf("w after is %f\n", w);
      //Rprintf("wtf |%f - %f| = |%f| = %f\n", survey[u][j], survey[v][j], survey[u][j] - survey[v][j], fabs(survey[u][j] - survey[v][j]));
      //Rprintf("%d %d %f difference was between %f %f\n", u, v, w, survey[u][j], survey[v][j]);
      ++count;
    }
  }
  //Rprintf("so...... w finally is %f\n", w);

  // Normalise by the number of valid comparisons.
  if(count > 0)
    w = 1.0 - w / double(count);

  // If the number of valid comparisons is less than a specified threshold,
  // set to -1, and this edge will not be included in edge list.
  if(count < comparisons)
    w = -1;
}

// Manhattan distance between rows or columns u and v
void surveygraph::dist_euclidean(const int &u, const int &v, double &w)
{
  int count = 0;  

  w = 0;
  for(unsigned int j = 0; j < survey[0].size(); ++j){
    if(!std::isnan(survey[u][j]) && !std::isnan(survey[v][j])){
      w += (survey[u][j] - survey[v][j]) * (survey[u][j] - survey[v][j]);
      ++count;
    }
  }

  if(count > 0)
    w = w / double(count);

  w = 1 - sqrt(w);

  if(count < comparisons)
    w = -1;
}
