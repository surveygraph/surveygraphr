#ifndef SURVEYGRAPH_H_
#define SURVEYGRAPH_H_

//#define R_NO_REMAP            // FIXME temporary
//#include <Rinternals.h>       // FIXME temporary
//#include <R_ext/Rdynload.h>   // FIXME temporary

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
      const int &rlayer,
      const int &rmethod,
      const double &rmethodval,
      const int &rmincomps,
      const int &rsimilarity
    ){
      survey = rdata;
      layer = rlayer;
      method = rmethod;
      methodval = rmethodval;
      mincomps = rmincomps;
      similarity = rsimilarity;
        
      nrow = int(survey.size());
      ncol = int(survey[0].size());

      if(method == 0) target_lcc = methodval;
      if(method == 1) target_ad  = methodval;
      if(method == 2) raw_similarity = methodval;

      if(method == 0) make_proj_lcc();
      if(method == 1) make_proj_ad();
      if(method == 2) make_proj_similar();

      //if(layer == 0 && method == 0) make_proj_agent_lcc();
      //if(layer == 0 && method == 1) make_proj_agent_ad();
      //if(layer == 0 && method == 2) make_proj_agent_similar();

      //if(layer == 1 && method == 0) make_proj_symbolic_lcc();
      //if(layer == 1 && method == 1) make_proj_symbolic_ad();
      //if(layer == 1 && method == 2) make_proj_symbolic_similar();

      g_dummy = g;
      //if(layer == 0) g_dummy = g_agent;
      //if(layer == 1) g_dummy = g_symbolic;
    }

    surveygraph(std::vector<std::vector<double>> &a){
      survey = a;
      nrow = int(survey.size());
      ncol = int(survey[0].size());  // will have verified dimensions in R routines
    }

    surveygraph(std::vector<std::vector<double>> &a, int b, double c, int d){
      survey = a;
      nrow = int(survey.size());
      ncol = int(survey[0].size());  // verify dimensions in R routines

      // b is threshold method flag
      if(b == 0){        // method is target lcc
        target_lcc = c;     
      }else if(b == 1){  // method is target avg degree
        target_ad = c;      
      }else if(b == 2){  // method is input raw similarity threshold
        raw_similarity = c;        
      }

      // d is similarity metric flag
      if(d == 0){        // similarity metric is Manhattan distance
        metric = 0;
      }
    }

    double target_lcc, target_ad, raw_similarity;
    int layer, method, mincomps, similarity, metric;
    double methodval;

    int nrow, ncol;  // number of agent, symbolic

    // survey, small sample of survey
    std::vector<std::vector<double>> survey, surveysample;

    graph g, g_dummy;
    //graph g_agent, g_symbolic, g_dummy;

    std::vector<std::vector<double>> profile;     // agent threshold data
    //std::vector<std::vector<double>> profile_agent;     // agent threshold data
    //std::vector<std::vector<double>> profile_symbolic;  // symbolic threshold data
    
    void search_threshold_lcc();
    void search_threshold_ad();

    //void search_threshold_agent_lcc();
    //void search_threshold_agent_ad();
    //void search_threshold_symbolic_lcc();
    //void search_threshold_symbolic_ad();

    void make_proj_lcc();      // builds agent projection graph with target largest component size
    void make_proj_ad();       // builds agent projection graph with target average degree
    void make_proj_similar();  // builds agent projection graph with desired threshold

    //void make_proj_agent_lcc();      // builds agent projection graph with target largest component size
    //void make_proj_agent_ad();       // builds agent projection graph with target average degree
    //void make_proj_agent_similar();  // builds agent projection graph with desired threshold

    //void make_proj_symbolic_lcc();      // builds symbolic projection graph with target largest component size
    //void make_proj_symbolic_ad();       // builds symbolic projection graph with target average degree
    //void make_proj_symbolic_similar();  // builds symbolic projection graph with desired threshold

    void max_threshold(double, int);
    //void max_threshold_agent(double, int);

    void make_threshold_profile();     // sweeps through a range of radii and studies 

    //void make_threshold_profile_agent();     // sweeps through a range of radii and studies 
    //void make_threshold_profile_symbolic();  // sweeps through a range of radii and studies 
};
#endif
