#ifndef EDGE_H_
#define EDGE_H_

#include <set>

#define R_NO_REMAP            // TODO comment out for CRAN, only used for debugging
#include <Rinternals.h>       // with Rprint.
#include <R_ext/Rdynload.h>   // 

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
#endif
