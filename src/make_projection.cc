#include "surveygraph.h"

#define R_NO_REMAP
#include "R.h"
#include "Rdefines.h"

using namespace std;

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

  // ensures target is in range [1, N], while methodval is in range [0, 1]
  int target = int(ceil(target_lcc * (survey.size() - 1))) + 1; 

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


void surveygraph::make_proj_ad()
{
  int emax = survey.size() * (survey.size() - 1) / 2;

  double eps = 1e-3;

  // Recall that average degree is non-increasing with the similarity threshold.
  double thresholdlower = -eps;     // LCC guaranteed to be survey.size()
  double thresholdupper = 1 + eps;  // LCC guaranteed to be 1
  int zlower = emax;                // we ensure elower >= target edge count
  int zupper = 0;                   // we ensure eupper < target edge count

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

void surveygraph::make_proj_similar()
{
  double threshold = raw_similarity;
  g = graph(threshold, mincomps, metric, survey); 
}
