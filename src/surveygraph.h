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
  double w;  // euclidean distance or cosine similarity weight

  bool operator<(const neighbour& rhs) const { return u < rhs.u; }
};

class surveygraph
{
  public :
    surveygraph(vector<vector<double>> &a){
      survey = a;
      m = survey.size();
      n = survey[0].size();  // will have verified dimensions in R routines
    }

    surveygraph(vector<vector<double>> &a, double b, double c){
      survey = a;
      m = survey.size();
      n = survey[0].size();  // will have verified dimensions in R routines

      lcc_respondents = b;
      lcc_items = c;
    }

    int m, n;  // number of respondents, items
    double avg_degree_respondents;
    double avg_degree_items;

    vector<vector<double>> survey, surveysample;  // survey, small sample of survey

    map<int, set<neighbour>> g_respondents;  // respondent graph
    map<int, set<neighbour>> g_items;        // item graph
    int e_respondents, e_items;              // number of edges

    vector<vector<double>> threshold_respondents;  // respondent threshold data
    vector<vector<double>> threshold_items;        // item threshold data
    
    double radius_respondents, radius_items;  // optimal radii for respondent and item graphs

    void search_radius_respondents();
    void search_radius_items();

    void graph_edgelists_pilot();  // builds a pair of graphs with optimal density
    void sweep_thresholds_pilot();  // sweeps through a range of radii and studies 

    void build_graph_items();
    void build_graph_respondents();

    void distance_items(const int&, const int&, double&);
    void distance_respondents(const int&, const int&, double&);

    // way too much repetition here, clean up later
    int lcc;
    double lcc_respondents, lcc_items;  // target LCC values, fraction of network
    set<vector<int>> partition_respondents, partition_items;
    void build_partition_respondents();  // computes distribution of component sizes
    void build_partition_items();        // computes distribution of component sizes
    void bfs_respondents(const int&, vector<int>&);   // breadth-first search
    void bfs_items(const int&, vector<int>&);         // breadth-first search
};
#endif
