#ifndef NEIGHBOUR_H_
#define NEIGHBOUR_H_

struct neighbour
{
  neighbour(int a, double b) { u = a; w = b; }

  int u;     // neighbour index
  double w;  // edge weight, corresponding to agent or symbolic similarity

  bool operator<(const neighbour& rhs) const { return u < rhs.u; }
};

#endif
