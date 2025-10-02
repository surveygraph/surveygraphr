#ifndef SURVEYGRAPH_H_
#define SURVEYGRAPH_H_

#include <vector>

//#ifndef R_NO_REMAP
//#define R_NO_REMAP
//#endif
//#include <R.h>  // TODO comment out for CRAN, only used for Rprint.

#include "edge.h"


class surveygraph
{
  public :
    // Constructor for make_projection()
    surveygraph(
      const std::vector<std::vector<double>> &rdata,
      const int &rmethod,
      const double &rmethodval,
      const int &rcomparisons,
      const int &rmetric
    ){
      survey = rdata;
      method = rmethod;
      methodval = rmethodval;
      comparisons = rcomparisons;
      metric = rmetric;

      if(method == 0){
        make_projection_lcc();
      }else if(method == 1){
        make_projection_avgdegree();
      }else if(method == 2){
        make_projection_similarity();
      }
    }

    // Constructor for make_threshold_profile()
    surveygraph(
      const std::vector<std::vector<double>> &rdata,
      const int &rcomparisons,
      const int &rmetric,
      const int &rcount
    ){
      survey = rdata;
      comparisons = rcomparisons;
      metric = rmetric;
      count = rcount;

      make_threshold_profile();
    }

    int method, comparisons, metric, count;
    double methodval;

    std::set<edge> edgelist;                   // edge list sorted by weight
    void edgelist_complete();                  // construct complete edgelist
    void edgelist_thresholded(const double&);  // construct thresholded edgelist
    void dist_manhattan(const int&, const int&, double&);
    void dist_euclidean(const int&, const int&, double&);

    std::vector<std::vector<double>> survey;
    std::vector<std::vector<int>> profile;     // data from threshold profile

    void make_projection_lcc();                // builds graph with target largest component size
    void make_projection_avgdegree();          // builds graph with target average degree
    void make_projection_similarity();         // builds graph with desired threshold
    void make_threshold_profile();             // sweeps through a range of radii and studies
};
#endif
