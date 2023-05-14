#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

/*
list_pilot() pilots the process of constructing respondent and item graphs. The
main loop searches for a value of the threshold that produces a largest
connected component, or LCC, that is a fraction lcc_respondents of the entire
graph.  For example, when lcc_respondents = 1, we search for a threshold value
that results in a fully connected graph.
*/
void surveygraph::graph_edgelists_pilot()
{
  radius_respondents = 0;

  double rlower = 0.0;
  double rupper = 1.0;
  int i = 0;
  bool rfound = false;
  while(i < 20 && !rfound){  // bisection method
    radius_respondents = (rlower + rupper) / 2.0;
    radius = radius_respondents;
    build_g_respondents();
    build_partition();

    if(lcc > int(lcc_respondents * m)){
      rupper = radius;
    }else if(lcc < int(lcc_respondents * m)){
      rlower = radius;
    }else if(lcc == int(lcc_respondents * m)){
      rfound = true;
    }
    i += 1;
    Rprintf("hello from list_pilot: %f %d %f\n", radius, lcc, zrespondents);
  }

  // build graph at desired radius
  radius = radius_respondents;
  build_g_respondents();
}
