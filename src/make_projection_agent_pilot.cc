#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

/*
make_proj_pilot() pilots the process of constructing respondent and item
graphs. The main loop searches for a value of the threshold that produces a
largest connected component, or LCC, that is a fraction lcc_agent of the
entire graph.  For example, when lcc_agent = 1, we search for a threshold
value that results in a fully connected graph.
*/
void surveygraph::make_proj_agent_lcc()
{
  search_threshold_agent_lcc();  // finds threshold_agent with desired lcc
  build_graph_agent();  // builds agent graph using the found threshold
}

void surveygraph::make_proj_agent_ad()
{
  search_threshold_agent_ad();  // finds threshold_agent with desired avg degree
  build_graph_agent();  // builds agent graph using the found threshold
}

void surveygraph::make_proj_agent_similar()
{
  threshold_agent = raw_similarity;
  build_graph_agent();  // builds agent graph using the found threshold
}

void surveygraph::search_threshold_agent_lcc()
{
  double tlower = -1;
  double tupper = 1;
  int lcclower = nrow;
  int lccupper = 0;

  threshold_agent = 0;

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 20){
    //Rprintf("i : %d, %f\n", i, threshold_agent);

    threshold_agent = (tlower + tupper) / 2.0;
    build_graph_agent();
    build_partition_agent();

    double lccdummy = lcc / double(nrow);

    if(lccdummy > target_lcc){
      tlower = threshold_agent;
    }else if(lccdummy < target_lcc){
      tupper = threshold_agent;
    }
    i += 1;
  }
}

void surveygraph::search_threshold_agent_ad()
{
  double tlower = -1;
  double tupper = 1;

  threshold_agent = 0;

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 20){  // bisection method
    threshold_agent = (tlower + tupper) / 2.0;
    build_graph_agent();
    //build_partition_agent();

    double addummy = avg_degree_agent / double(nrow);

    if(addummy > target_ad){
      tlower = threshold_agent;
    }else if(addummy < target_ad){
      tupper = threshold_agent;
    }
    i += 1;
  }
}
