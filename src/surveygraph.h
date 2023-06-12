#ifndef SURVEYGRAPH_H_
#define SURVEYGRAPH_H_

#include <vector>
#include <map>
#include <set>

using namespace std;

struct neighbour
{
  neighbour(int a, double b) { u = a; w = b; }

  int u;     // neighbour index
  double w;  // edge weight, corresponding to agent or symbolic similarity

  bool operator<(const neighbour& rhs) const { return u < rhs.u; }
};

class surveygraph
{
  public :
    surveygraph(vector<vector<double>> &a){
      survey = a;
      nrow = survey.size();
      ncol = survey[0].size();  // will have verified dimensions in R routines
    }

    surveygraph(vector<vector<double>> &a, int b, double c, int d){
      survey = a;
      nrow = survey.size();
      ncol = survey[0].size();  // verify dimensions in R routines

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
    int metric;

    int nrow, ncol;  // number of agent, symbolic
    double avg_degree_agent;
    double avg_degree_symbolic;

    vector<vector<double>> survey, surveysample;  // survey, small sample of survey

    map<int, set<neighbour>> g_agent;     // agent graph
    map<int, set<neighbour>> g_symbolic;  // symbolic graph
    int e_agent, e_symbolic;              // number of edges

    vector<vector<double>> threshold_data_agent;     // agent threshold data
    vector<vector<double>> threshold_data_symbolic;  // symbolic threshold data
    
    double threshold_agent, threshold_symbolic;  // optimal radii for agent and symbolic graphs

    void search_threshold_agent_lcc();
    void search_threshold_agent_ad();
    void search_threshold_symbolic_lcc();
    void search_threshold_symbolic_ad();

    void make_proj_agent_lcc();      // builds agent projection graph with target largest component size
    void make_proj_agent_ad();       // builds agent projection graph with target average degree
    void make_proj_agent_similar();  // builds agent projection graph with desired threshold

    void make_proj_symbolic_lcc();      // builds symbolic projection graph with target largest component size
    void make_proj_symbolic_ad();       // builds symbolic projection graph with target average degree
    void make_proj_symbolic_similar();  // builds symbolic projection graph with desired threshold

    void max_threshold_agent(double, int);

    void make_threshold_profile_agent();     // sweeps through a range of radii and studies 
    void make_threshold_profile_symbolic();  // sweeps through a range of radii and studies 

    void build_graph_symbolic();
    void build_graph_agent();

    void euclid_distance_symbolic(const int&, const int&, double&);
    void euclid_distance_agent(const int&, const int&, double&);

    void man_distance_symbolic(const int&, const int&, double&);
    void man_distance_agent(const int&, const int&, double&);

    // way too much repetition here, clean up later
    int lcc;
    double lcc_agent, lcc_symbolic;  // target LCC values, fraction of network
    int isol_agent, isol_symbolic;
    int comp_agent, comp_symbolic;
    set<vector<int>> partition_agent, partition_symbolic;
    void build_partition_agent();  // computes distribution of component sizes
    void build_partition_symbolic();        // computes distribution of component sizes
    void bfs_agent(const int&, vector<int>&);   // breadth-first search
    void bfs_symbolic(const int&, vector<int>&);         // breadth-first search
};
#endif
