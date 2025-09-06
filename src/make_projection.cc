#include "surveygraph.h"

#define R_NO_REMAP
#include "R.h"
#include "Rdefines.h"

using namespace std;

void surveygraph::make_proj_lcc()
{
  search_threshold_lcc();  // finds threshold with desired lcc
}

void surveygraph::make_proj_ad()
{
  search_threshold_ad();  // finds threshold with desired avg degree
}

void surveygraph::make_proj_similar()
{
  double threshold = raw_similarity;
  g = graph(0, threshold, mincomps, survey); 
}

// find the largest threshold for which the observed lcc is as close 
// as possible to the target lcc
void surveygraph::search_threshold_lcc()
{
  double tlower = -1;
  double tupper = 1;
  int lcclower = nrow;
  int lccupper = 1;

  // important to round here, rather than take floor or ceiling
  int target = int(round(target_lcc * double(nrow))); 

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 15){
    double threshold = (tlower + tupper) / 2.0;

    g = graph(0, threshold, mincomps, survey);

    if(g.lcc > target){
      tlower = threshold;
      lcclower = g.lcc;
    }else if(g.lcc < target){
      tupper = threshold;
      lccupper = g.lcc;
    }else{ // TODO split into two cases, upper bound vs lower bound on plateau
      tfound = true;  // lcc == target 
      tlower = threshold;
      lcclower = target;  // this is arbitrary
    }
    i += 1;
  }

  // five cases, maximise the threshold along the desired plateau
  if(lcclower == target){
    max_threshold(tlower, lcclower);
  }else if(lccupper == target){
    max_threshold(tupper, lccupper);
  }else if(abs(target - lccupper) < abs(target - lcclower)){
    max_threshold(tupper, lccupper);
  }else if(abs(target - lcclower) < abs(target - lccupper)){
    max_threshold(tlower, lcclower);
  }else if(abs(target - lcclower) == abs(target - lccupper)){
    max_threshold(tupper, lccupper);
  }
}

// find largest threshold that produces an lcc of size l
void surveygraph::max_threshold(double t, int l)
{
  double tlower = t;
  double tupper = 1;
  double threshold;
  int i = 0;
  while(i < 15){
    threshold = (tlower + tupper) / 2.0;

    g = graph(0, threshold, mincomps, survey);

    if(g.lcc != l){
      tupper = threshold;
    }else if(g.lcc == l){
      tlower = threshold;
    }
    i += 1;
  }
  threshold = tlower;

  g = graph(0, threshold, mincomps, survey);

  //if(l != g.lcc){
  //  error("an internal test has failed, please report to package creators\n");
  //}
}

void surveygraph::search_threshold_ad()
{
  double tlower = -1;
  double tupper = 1;

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 20){  // bisection method
    double threshold = (tlower + tupper) / 2.0;
    g = graph(0, threshold, mincomps, survey);

    double addummy = g.avg_degree / double(g.network.size());

    if(addummy > target_ad){
      tlower = threshold;
    }else if(addummy < target_ad){
      tupper = threshold;
    }
    i += 1;
  }
}
