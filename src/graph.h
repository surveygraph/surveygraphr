#ifndef GRAPH_H_
#define GRAPH_H_

#include <vector>
#include <map>
#include <set>

//#define R_NO_REMAP            // FIXME temporary
//#include <Rinternals.h>       // FIXME temporary
//#include <R_ext/Rdynload.h>   // FIXME temporary

using namespace std;

typedef std::vector<std::vector<double>> survey;
//enum class Layer{agent, symbolic};

struct neighbour
{
  neighbour(int a, double b) { u = a; w = b; }

  int u;     // neighbour index
  double w;  // edge weight, corresponding to agent or symbolic similarity

  bool operator<(const neighbour& rhs) const { return u < rhs.u; }
};

class graph
{
  public :
    graph(){}

    // Arguments are everything you might need to produce an edge from a
    // comparison of two rows from survey.
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

    // provide flag for projection type, threshold, and survey data
    //graph(const int &a, const double &b, const int &c, const survey &S){
    //  switch(a){
    //    case 0:
    //      f = 0;
    //      layer = Layer::agent;
    //      n = int(S.size());          
    //      m = int(S[0].size());          
    //      break;
    //    case 1:
    //      f = 1;
    //      layer = Layer::symbolic;
    //      m = int(S.size());
    //      n = int(S[0].size());
    //      break;
    //    default:
    //      f = 0;
    //      layer = Layer::agent;
    //      n = int(S.size());
    //      m = int(S[0].size());
    //  }
    //  threshold = b;
    //  mincomps = c;

    //  build_graph(S);
    //  build_partition();
    //}

    double threshold;  // keep an edge if its similarity is above this threshold
    int mincomps;      // minimum number of valid comparisons for an edge to be counted
    int metric;        // flag describing metric used to compute distance (Manhattan Euclidean, ...)

    //int n;  // number of graph nodes (nrow if agent, ncol if symbolic)
    int e;  // number of graph edges
    //Layer layer;
    //int m;  // complements n (ncol if agent, nrow if symbolic)
    //int f;  // 0 for agent, 1 for symbolic, projection flag

    std::map<int, std::set<neighbour>> network;  // neighbour list

    // topological properties of the resultant network
    int lcc;                               // size of largest connected component
    int isols;                             // number of isolated nodes
    int comps;                             // number of components
    double avg_degree;                     // average degree
    std::set<std::vector<int>> partition;  // partition of nodes according to components

    void build_graph(const survey&);       // constructs graphs by comparing all survey row pairs
    void build_partition();                   // computes distribution of component sizes
    void bfs(const int&, std::vector<int>&);  // breadth-first search

    void dist_manhattan(const survey&, const int&, const int&, double&);
    void dist_euclidean(const survey&, const int&, const int&, double&);
    //void man_distance(const survey&, const int&, const int&, double&);
    //void euclid_distance(const survey&, const int&, const int&, double&);
};
#endif
