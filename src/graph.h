#ifndef GRAPH_H_
#define GRAPH_H_

#include <vector>
#include <map>
#include <set>

#define R_NO_REMAP            // TODO comment out for CRAN, only used for debugging
#include <Rinternals.h>       // with Rprint.
#include <R_ext/Rdynload.h>   // 

using namespace std;

typedef std::vector<std::vector<double>> survey;

struct neighbour
{
  neighbour(int index, double w) : u(index), weight(w) {}

  int u;
  double weight;

  bool operator<(const neighbour& rhs)const{return u < rhs.u;}
};

struct edge
{
  edge(std::set<int> n, double w) : nodes(std::move(n)), weight(w) {}

  std::set<int> nodes;
  double weight;

  bool operator<(const edge& rhs)const{
    if(weight != rhs.weight)
      return weight < rhs.weight;
    else
      return nodes < rhs.nodes;
  }
};

class graph
{
  public :
    graph(){}

    // used mostly in make_projection()
    graph(
      const double &a,    // threshold
      const int &b,       // mincomps
      const int &c,       // metric
      const survey &S     // survey data
    ){
      threshold = a;
      mincomps = b;
      metric = c;

      build_graph(S);
      build_partition(); 
    }

    graph(
      const int &a,       // mincomps
      const int &b,       // metric
      const survey &S     // survey data
    ){
      mincomps = a;
      metric = b;

      build_complete(S);
    }

    double threshold;  // keep an edge if its similarity is above this threshold
    int mincomps;      // minimum number of valid comparisons for an edge to be counted
    int metric;        // flag describing metric used to compute distance

    std::map<int, std::set<neighbour>> network;  // neighbour list
    std::set<edge> edgelist;                     // edge list sorted by weight

    // topological properties of graph
    int lcc;                                  // size of largest connected component
    int isols;                                // number of isolated nodes
    int comps;                                // number of components
    int e;                                    // number of edges
    double avg_degree;                        // average degree
    std::set<std::vector<int>> partition;     // partition of nodes according to components

    void build_graph(const survey&);          // constructs graphs by comparing all survey row pairs
    void build_complete(const survey&);       // constructs a complete edge list
    void build_partition();                   // computes distribution of component sizes
    void bfs(const int&, std::vector<int>&);  // breadth-first search

    void dist_manhattan(const survey&, const int&, const int&, double&);
    void dist_euclidean(const survey&, const int&, const int&, double&);
};
#endif
