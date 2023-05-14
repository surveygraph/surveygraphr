#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

/*
list_pilot() pilots the process of constructing respondent and item graphs. The
main loop searches for a value of the threshold that produces a largest
connected component, or LCC, that is a fraction lcctarget of the entire graph.
For example, when lcctarget = 1, we search for a threshold value that results
in a fully connected graph.
*/
void surveygraph::list_pilot()
{
  radius_respondents = 0;
  int lccopt = int(1e6);
  lcctarget = 1.01;

  // search for radius giving lcc closest to 0.5
  for(double r = 0; r < 0.5; r += 0.001){
    radius = r;
    build_g_respondents();
    build_partition();

    if(abs(lcc - int(lcctarget * m)) < lccopt){
      lccopt = abs(lcc - int(lcctarget * m));
      radius_respondents = radius;
    }
  }
  // build graph at desired radius
  radius = radius_respondents;
  build_g_respondents();
}
