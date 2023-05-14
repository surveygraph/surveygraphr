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
      n = survey[0].size();      // will have verified dimensions in R routines
    }

    surveygraph(vector<vector<double>> &a, double b, double c){
      survey = a;
      m = survey.size();
      n = survey[0].size();      // will have verified dimensions in R routines

      lcc_respondents = b;
      lcc_items = c;
    }

    int m, n;                     // number of respondents, items
    int edgecountr, edgecounti;   // number of edges in respondent and item graphs
    double threshold;             // used as an edge cutoff, ranges from 0 to 2 * sqrt m
    double zrespondents, zitems;  // average degree of each graph
    double average_degree_respondents;
    double average_degree_items;

    vector<vector<double>> survey, surveysample;  // survey, small sample of survey

    map<int, set<neighbour>> g_respondents;  // respondent graph
    map<int, set<neighbour>> g_items;        // item graph
    
    double radius, radius_respondents, radius_items;  // optimal radii for respondent and item graphs
    vector<vector<double>> explore_respondents;

    void search_threshold_respondents();
    void search_threshold_items();

    void graph_edgelists_pilot();  // builds a pair of graphs with optimal density
    void explore_pilot();  // sweeps through a range of radii and studies 

    void build_g_items();
    void build_g_respondents();

    int lcc;
    double lcc_respondents, lcc_items;  // target LCC values, fraction of network
    double lcctarget;
    set<vector<int>> partition;
    void build_partition();               // computes distribution of component sizes
    void bfs(const int&, vector<int>&);   // breadth-first search

    void item_euclid(const int&, const int&, double&);
    void respondent_euclid(const int&, const int&, double&);
};
#endif
