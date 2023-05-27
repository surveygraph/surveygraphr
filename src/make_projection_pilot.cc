#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

/*
make_projection_pilot() pilots the process of constructing respondent and item
graphs. The main loop searches for a value of the radius that produces a
largest connected component, or LCC, that is a fraction lcc_respondents of the
entire graph.  For example, when lcc_respondents = 1, we search for a radius
value that results in a fully connected graph.
*/
void surveygraph::make_projection()
{
  search_radius_respondents();  // sets radius_respondents
  build_graph_respondents();    // builds respondent graph using the found radius

  search_radius_items();        // sets radius_items
  build_graph_items();          // builds item graph using the found radius
}

void surveygraph::make_projection_agent()
{
  search_radius_respondents();  // sets radius_respondents
  build_graph_respondents();    // builds respondent graph using the found radius
}

void surveygraph::make_projection_symbolic()
{
  search_radius_items();        // sets radius_items
  build_graph_items();          // builds item graph using the found radius
}

void surveygraph::search_radius_respondents()
{
  radius_respondents = 0;
  //double rlower = -ncol;
  //double rupper = ncol;
  double rlower = 0;
  double rupper = 2 * ncol;
  bool rfound = false;
  int i = 0;
  while(!rfound && i < 20){  // bisection method
    radius_respondents = (rlower + rupper) / 2.0;
    build_graph_respondents();
    build_partition_respondents();

    if(lcc > int(lcc_respondents * nrow)){
      rupper = radius_respondents;
    }else if(lcc < int(lcc_respondents * nrow)){
      rlower = radius_respondents;
    }else if(lcc == int(lcc_respondents * nrow)){
      rfound = true;
    }
    i += 1;
  }
}

void surveygraph::search_radius_items()
{
  radius_items = 0;
  //double rlower = -nrow;
  //double rupper = nrow;
  double rlower = 0;
  double rupper = 2 * nrow;
  bool rfound = false;
  int i = 0;
  while(!rfound && i < 20){  // bisection method
    radius_items = (rlower + rupper) / 2.0;
    build_graph_items();
    build_partition_items();

    if(lcc > int(lcc_items * ncol)){ // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< rename to target_lcc_items
      rupper = radius_items;
    }else if(lcc < int(lcc_items * ncol)){ // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< rename to target_lcc_items
      rlower = radius_items;
    }else if(lcc == int(lcc_items * ncol)){ // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< rename to target_lcc_items
      rfound = true;
    }
    i += 1;
  }
}
