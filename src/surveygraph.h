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
    surveygraph(){}
    surveygraph(vector<vector<double>> &s){
      surveyvec = s;
      m = surveyvec.size();
      n = surveyvec[0].size();      // will have verified dimensions in R routines
    }

    int m, n;                     // number of respondents, items
    int edgecountr, edgecounti;   // number of edges in respondent and item graphs
    double threshold, radius;     // used as an edge cutoff, ranges from 0 to 2 * sqrt m
    double zrespondents, zitems;  // average degree of each graph

    vector<vector<double>> surveyvec;        // survey in vector format
    map<int, set<neighbour>> g_respondents;  // respondent graph
    map<int, set<neighbour>> g_items;        // item graph
    
    double radius_respondents, radius_items;  // optimal radii for respondent and item graphs
    void optimise_radii();
    vector<vector<double>> explore_respondents;

    void list_pilot();      // builds a pair of graphs with optimal density
    void explore_pilot();   // sweeps through a range of radii and studies 

    void build_g_items();
    void build_g_respondents();

    int lcc;
    double lcctarget;
    set<vector<int>> partition;
    void build_partition();               // computes distribution of component sizes
    void bfs(const int&, vector<int>&);   // breadth-first search

    void item_euclid(const int&, const int&, double&);
    void respondent_euclid(const int&, const int&, double&);
};
#endif
