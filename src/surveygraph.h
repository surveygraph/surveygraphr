#ifndef SURVEYGRAPH_H_
#define SURVEYGRAPH_H_

// FIXME remove this when you no longer need Rprintf, ie when you finish debugging
#define R_NO_REMAP            // FIXME temporary
#include <Rinternals.h>       // FIXME temporary
#include <R_ext/Rdynload.h>   // FIXME temporary

#include "graph.h"

#include <vector>
#include <map>
#include <set>
#include <cmath>

#include <iostream> // FIXME temporary

class surveygraph
{
  public :
    surveygraph(
      const std::vector<std::vector<double>> &rdata,
      const int &rmethod,
      const double &rmethodval,
      const int &rmincomps,
      const int &rmetric
    ){
      survey = rdata;
      method = rmethod;
      methodval = rmethodval;
      mincomps = rmincomps;
      metric = rmetric;
      
      //Rprintf("hello from surveygraph constructor, method is %d\n", method);

      if(method == 0){
        target_lcc = methodval;
        make_proj_lcc();
      }else if(method == 1){
        target_ad  = methodval;
        make_proj_ad();
      }else if(method == 2){
        raw_similarity = methodval;
        make_proj_similar();
      }

      g_dummy = g;
    }

    surveygraph(std::vector<std::vector<double>> &a){
      survey = a;
    }

    int method, mincomps, metric;
    double methodval;

    double target_lcc, target_ad, raw_similarity;

    // survey, small sample of survey
    std::vector<std::vector<double>> survey, surveysample;

    graph g, g_dummy;

    std::vector<std::vector<double>> profile;     // agent threshold data
    
    //void search_threshold_lcc();
    //void search_threshold_ad();

    void make_proj_lcc();      // builds agent projection graph with target largest component size
    void make_proj_ad();       // builds agent projection graph with target average degree
    void make_proj_similar();  // builds agent projection graph with desired threshold

    //void max_threshold(double, int);
    //void max_threshold_agent(double, int);

    void make_threshold_profile();     // sweeps through a range of radii and studies 

    //void make_threshold_profile_agent();     // sweeps through a range of radii and studies 
    //void make_threshold_profile_symbolic();  // sweeps through a range of radii and studies 
};
#endif
