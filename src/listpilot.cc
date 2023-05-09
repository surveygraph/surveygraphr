#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

// pilots the process of constructing respondent and item graphs
void surveygraph::list_pilot()
{
  // search for radius / threshold, s.t. lcc is close to 0.5
  radius_respondents = 0;
  int lccopt = int(1e6);

  // search for radius giving lcc closest to 0.5
  for(double r = 0; r < 0.5; r += 0.001){
    radius = r;
    build_g_respondents();
    build_partition();

    if(abs(lcc - m / 2) < lccopt){
      lccopt = abs(lcc - m / 2);
      radius_respondents = radius;
    }
  }

  // build graph at desired radius
  radius = radius_respondents;
  build_g_respondents();
  Rprintf("optimal radius is %f\n", radius_respondents);
}
