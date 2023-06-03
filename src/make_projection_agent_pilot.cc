#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

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

// find the largest threshold for which the observed lcc is as close 
// as possible to the target lcc
void surveygraph::search_threshold_agent_lcc()
{
  double tlower = -1;
  double tupper = 1;
  int lcclower = nrow;
  int lccupper = 1;

  // important to round here, rather than take ciel or floor
  int target = int(round(target_lcc * double(nrow))); 

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 15){
    threshold_agent = (tlower + tupper) / 2.0;
    build_graph_agent();
    build_partition_agent();

    if(lcc > target){
      tlower = threshold_agent;
      lcclower = lcc;
    }else if(lcc < target){
      tupper = threshold_agent;
      lccupper = lcc;
    }else{ 
      tfound = true;  // lcc == target 
      tlower = threshold_agent;
      lcclower = target;  // this is arbitrary
    }
    i += 1;
  }

  // maximise threshold along the found plateau
  if(lcclower == target){
    max_threshold_agent(tlower, lcclower);
  }else if(lccupper == target){
    max_threshold_agent(tupper, lccupper);
  }else if(abs(target - lccupper) < abs(target - lcclower)){
    max_threshold_agent(tupper, lccupper);
  }
}

// find largest threshold that produces an lcc of size l
void surveygraph::max_threshold_agent(double t, int l)
{
  double tlower = t;
  double tupper = 1;
  int i = 0;
  while(i < 15){
    threshold_agent = (tlower + tupper) / 2.0;
    build_graph_agent();
    build_partition_agent();

    if(lcc != l){
      tupper = threshold_agent;
    }else if(lcc == l){
      tlower = threshold_agent;
    }
    i += 1;
  }
  threshold_agent = tlower;
  build_graph_agent();
  build_partition_agent();

  if(l != lcc){
    error("an internal test has failed, please report to package creators\n");
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
