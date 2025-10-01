#ifndef EDGE_H_
#define EDGE_H_

#include <set>

#define R_NO_REMAP
#include <R.h>  // TODO comment out for CRAN, only used for Rprint.


// TODO you could be using uint16_t, if survey.size() \leq 65535. should be a
// template that assigns type at runtime.
// TODO there's no way you need double precision for the weights, floats are fine
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
