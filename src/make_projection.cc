#include "surveygraph.h"

#define R_NO_REMAP
#include "R.h"
#include "Rdefines.h"

using namespace std;

void surveygraph::make_proj_lcc()
{
  search_threshold_lcc();  // finds threshold with desired LCC
}

void surveygraph::make_proj_ad()
{
  search_threshold_ad();  // finds threshold with desired avg degree
}

void surveygraph::make_proj_similar()
{
  double threshold = raw_similarity;
  g = graph(threshold, mincomps, metric, survey); 
}

// Find the largest threshold for which the resulting LCC is greater than or
// equal to the target LCC. Of course, the set of LCCs is degenerate; there can be
// many thresholds that produce identical graphs with LCCs of the same size. We
// choose the largest possible threshold so that the network is as sparse as
// possible.
//
// Note that we could take advantage of the fact that in small graphs, the
// set of edge weights, or similarities, will be small, meaning we can avoid
// unnecessary halving-the-interval steps below. However, since small graphs 
// are fast to compute anyway, we don't bother.
void surveygraph::search_threshold_lcc()
{
  //Rprintf("helloooooo from search_threshold_lcc\n");

  // Recall that LCC is non-increasing with the similarity threshold.
  double thresholdlower = -0.001;  // LCC guaranteed to be survey.size()
  double thresholdupper =  1.001;  // LCC guaranteed to be 1
  int lcclower = survey.size();    // we ensure lcclower >= target LCC    
  int lccupper = 1;                // we ensure lccupper < target LCC

  // special case: survey.size() is 0 or 1
  // special case: target is 0 or 1

  int target = int(ceil(target_lcc * double(survey.size()))); 

  if(target == 0 || target == 1 || survey.size() == 1){
    g = graph(1.0001, mincomps, metric, survey); 
    return;
  }

  int count = 0;
  while(count < 10){
    double threshold = (thresholdlower + thresholdupper) / 2.0;

    g = graph(threshold, mincomps, metric, survey); 
    
    if(g.lcc >= target)
      thresholdlower = threshold;
    else
      thresholdupper = threshold;

    count += 1;
  }
  g = graph(thresholdlower, mincomps, metric, survey); 
}

//void surveygraph::search_threshold_lcc()
//{
//  //double tlower = -1;
//  double tlower = -0.001;
//  double tupper =  1.001;
//
//  // these might appear the wrong way around, but it's because the size of the
//  // LCC decreases with the similarity threshold.
//  int lcclower = survey.size();
//  int lccupper = 1;
//
//  // important to round here, rather than take floor or ceiling. (why?)
//  int target = int(round(target_lcc * double(survey.size()))); 
//
//  bool tfound = false;
//  int count = 0;
//  while(!tfound && count < 15){
//    double threshold = (tlower + tupper) / 2.0;
//
//    g = graph(threshold, mincomps, metric, survey); 
//
//    // two steps or cases, first, y
//    if(g.lcc > target){
//      tlower = threshold;
//      lcclower = g.lcc;
//    }else if(g.lcc < target){
//      tupper = threshold;
//      lccupper = g.lcc;
//    }else{
//      // TODO split into two cases
//      tfound = true;      
//      tlower = threshold;
//      lcclower = target;
//    }
//    count += 1;
//  }
//
//  // five cases, maximise the threshold along the desired plateau
//
//  if(lcclower == target){                                       // lower lcc matches target
//    max_threshold(tlower, lcclower);
//  }else if(lccupper == target){                                 // upper lcc matches target
//    max_threshold(tupper, lccupper);
//  }else if(abs(target - lccupper) < abs(target - lcclower)){    // lower lcc is closer to target
//    max_threshold(tupper, lccupper);
//  }else if(abs(target - lcclower) < abs(target - lccupper)){    // upper lcc is closer to target
//    max_threshold(tlower, lcclower);
//  }else if(abs(target - lcclower) == abs(target - lccupper)){   // lower and upper lccs are equidistant from target
//    max_threshold(tupper, lccupper);
//  }
//}

// Find the largest threshold possible that produces an LCC of size l.
//void surveygraph::max_threshold(double t, int l)
//{
//  double tlower = t;
//  double tupper = 1;
//  double threshold;
//  int count = 0;
//  while(count < 15){
//    //Rprintf("hello from second searching loop: %d of 15 at most\n", i);
//    threshold = (tlower + tupper) / 2.0;
//
//    g = graph(threshold, mincomps, metric, survey);
//
//    if(g.lcc != l){
//      tupper = threshold;
//    }else if(g.lcc == l){
//      tlower = threshold;
//    }
//    count += 1;
//  }
//  threshold = tlower;
//
//  g = graph(threshold, mincomps, metric, survey);
//  //if(l != g.lcc){
//  //  error("an internal test has failed, please report to package creators\n");
//  //}
//}

void surveygraph::search_threshold_ad()
{
  double tlower = -1;
  double tupper = 1;

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 20){  // bisection method
    double threshold = (tlower + tupper) / 2.0;
    g = graph(threshold, mincomps, metric, survey);

    double addummy = g.avg_degree / double(g.network.size());

    if(addummy > target_ad){
      tlower = threshold;
    }else if(addummy < target_ad){
      tupper = threshold;
    }
    i += 1;
  }
}
