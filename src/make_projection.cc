#include "surveygraph.h"

#define R_NO_REMAP
#include "R.h"
#include "Rdefines.h"

using namespace std;

//void surveygraph::make_proj_lcc()
//{
//  search_threshold_lcc();  // finds threshold with desired LCC
//}

//void surveygraph::make_proj_ad()
//{
//  search_threshold_ad();  // finds threshold with desired avg degree
//}

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
//void surveygraph::search_threshold_lcc()
void surveygraph::make_proj_lcc()
{
  // Recall that LCC is non-increasing with the similarity threshold.
  double thresholdlower = -0.001;  // LCC guaranteed to be survey.size()
  double thresholdupper =  1.001;  // LCC guaranteed to be 1
  int lcclower = survey.size();    // we ensure lcclower >= target LCC    
  int lccupper = 1;                // we ensure lccupper < target LCC

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


//void surveygraph::search_threshold_ad()
void surveygraph::make_proj_ad()
{
  // Recall that average degree is non-increasing with the similarity threshold.
  double thresholdlower = -0.001;  // LCC guaranteed to be survey.size()
  double thresholdupper =  1.001;  // LCC guaranteed to be 1

  int emax = survey.size() * (survey.size() - 1) / 2;
  int zlower = emax;               // we ensure elower >= target edge count
  int zupper = 0;                  // we ensure eupper < target edge count

  int target = int(ceil(target_ad * emax)); 

  if(target == 0 || survey.size() == 1){
    g = graph(1.0001, mincomps, metric, survey); 
    return;
  }

  int count = 0;
  while(count < 10){
    double threshold = (thresholdlower + thresholdupper) / 2.0;

    g = graph(threshold, mincomps, metric, survey); 
    
    if(g.e >= target)
      thresholdlower = threshold;
    else
      thresholdupper = threshold;

    count += 1;
  }
  g = graph(thresholdlower, mincomps, metric, survey); 
}

//void surveygraph::search_threshold_ad()
//{
//  double tlower = -1;
//  double tupper = 1;
//
//  bool tfound = false;
//  int i = 0;
//  while(!tfound && i < 20){  // bisection method
//    double threshold = (tlower + tupper) / 2.0;
//    g = graph(threshold, mincomps, metric, survey);
//
//    double addummy = g.avg_degree / double(g.network.size());
//
//    if(addummy > target_ad){
//      tlower = threshold;
//    }else if(addummy < target_ad){
//      tupper = threshold;
//    }
//    i += 1;
//  }
//}

void surveygraph::make_proj_similar()
{
  double threshold = raw_similarity;
  g = graph(threshold, mincomps, metric, survey); 
}
