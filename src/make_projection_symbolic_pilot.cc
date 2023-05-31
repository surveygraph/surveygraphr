#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

void surveygraph::make_proj_symbolic_lcc()
{
  search_threshold_symbolic_lcc();  // finds threshold_symbolic with desired lcc
  build_graph_symbolic();  // builds symbolic graph using the found threshold
}

void surveygraph::make_proj_symbolic_ad()
{
  search_threshold_symbolic_ad();  // finds threshold_symbolic with desired avg degree
  build_graph_symbolic();  // builds symbolic graph using the found threshold
}

void surveygraph::make_proj_symbolic_similar()
{
  threshold_symbolic = raw_similarity;
  build_graph_symbolic();  // builds symbolic graph using the found threshold
}

void surveygraph::search_threshold_symbolic_lcc()
{
  double tlower = -1;
  double tupper = 1;

  threshold_symbolic = 0;

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 20){  // bisection method
    threshold_symbolic = (tlower + tupper) / 2.0;
    build_graph_symbolic();
    build_partition_symbolic();

    double lccdummy = lcc / double(ncol);

    if(lccdummy > target_lcc){
      tlower = threshold_symbolic;
    }else if(lccdummy < target_lcc){
      tupper = threshold_symbolic;
    }else if(lccdummy == target_lcc){
      tfound = true;
    }
    i += 1;
  }
}

void surveygraph::search_threshold_symbolic_ad()
{
  double tlower = -1;
  double tupper = 1;

  threshold_symbolic = 0;

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 20){  // bisection method
    threshold_symbolic = (tlower + tupper) / 2.0;
    build_graph_symbolic();
    //build_partition_symbolic();

    double addummy = avg_degree_symbolic / double(ncol);

    if(addummy > target_ad){
      tlower = threshold_symbolic;
    }else if(addummy < target_ad){
      tupper = threshold_symbolic;
    }else if(addummy == target_ad){
      tfound = true;
    }
    i += 1;
  }
}
