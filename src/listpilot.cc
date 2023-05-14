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
  lcctarget = 0.99;
  int lccopt = int(1e6);

  // halving the interval
  int searchmax = 20;
  double rlower = 0.0;
  double rupper = 1.0;
  int i = 0;
  while(i < 20){
    radius_respondents = (rlower + rupper) / 2.0;
    radius = radius_respondents;
    build_g_respondents();
    build_partition();

    if(lcc > int(lcctarget * m)){
      rupper = radius;
    }else if(lcc < int(lcctarget * m)){
      rlower = radius;
    }else if(lcc == int(lcctarget * m)){
      Rprintf("exit bisection method\n");
    }
    i += 1;
    Rprintf("hello from list_pilot: %f %d %f\n", radius, lcc, zrespondents);
  }

  //for(double r = 0.1; r < 0.4; r += 0.005){
  //  radius = r;
  //  build_g_respondents();
  //  build_partition();

  //  if(abs(lcc - int(lcctarget * m)) < lccopt){
  //    lccopt = abs(lcc - int(lcctarget * m));
  //    radius_respondents = radius;
  //  }
  //  //Rprintf("hello from list_pilot: %f %d %f\n", r, lcc, zrespondents);
  //}

  // build graph at desired radius
  radius = radius_respondents;
  build_g_respondents();
}
