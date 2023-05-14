#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

/*
list_pilot() pilots the process of constructing respondent and item graphs. The
main loop searches for a value of the radius that produces a largest connected
component, or LCC, that is a fraction lcc_respondents of the entire graph.  For
example, when lcc_respondents = 1, we search for a radius value that results in
a fully connected graph.
*/
void surveygraph::graph_edgelists_pilot()
{
  search_radius_respondents();  // sets radius_respondents
  build_graph_respondents();    // builds respondent graph using the found radius

  search_radius_items();        // sets radius_items
  build_graph_items();          // builds item graph using the found radius

  Rprintf("zrespond %f ", avg_degree_respondents);
  Rprintf("radrespond %f ", radius_respondents);
  Rprintf("zitem %f ", avg_degree_items);
  Rprintf("raditem %f ", radius_items);
  Rprintf("\n");
  Rprintf("\n");

  for(int i = 0; i < 5; ++i){
    Rprintf("%d : ", i);
    for(auto it : g_respondents[i]){
      Rprintf("%d [%f] ", it.u, it.w);
    }
    Rprintf("\n");
  }
  Rprintf("\n");

  for(int i = 0; i < 5; ++i){
    Rprintf("%d : ", i);
    for(auto it : g_items[i]){
      Rprintf("%d [%f] ", it.u, it.w);
    }
    Rprintf("\n");
  }
}

void surveygraph::search_radius_respondents()
{
  radius_respondents = 0;
  double rlower = 0.0;
  double rupper = 1.0;
  bool rfound = false;
  int i = 0;
  while(!rfound && i < 20){  // bisection method
    radius_respondents = (rlower + rupper) / 2.0;
    build_graph_respondents();
    build_partition();

    if(lcc > int(lcc_respondents * m)){
      rupper = radius_respondents;
    }else if(lcc < int(lcc_respondents * m)){
      rlower = radius_respondents;
    }else if(lcc == int(lcc_respondents * m)){
      rfound = true;
    }
    i += 1;
    //Rprintf("hello from list_pilot: %f %d %f\n", radius_respondents, lcc, avg_degree_respondents);
  }
}

void surveygraph::search_radius_items()
{
  radius_items = 0;
  double rlower = 0.0;
  double rupper = 1.0;
  bool rfound = false;
  int i = 0;
  while(!rfound && i < 20){  // bisection method
    radius_items = (rlower + rupper) / 2.0;
    build_graph_items();
    build_partition();

    if(lcc > int(lcc_items * m)){
      rupper = radius_items;
    }else if(lcc < int(lcc_items * m)){
      rlower = radius_items;
    }else if(lcc == int(lcc_items * m)){
      rfound = true;
    }
    i += 1;
    //Rprintf("hello from list_pilot: %f %d %f\n", radius_items, lcc, avg_degree_items);
  }
}
