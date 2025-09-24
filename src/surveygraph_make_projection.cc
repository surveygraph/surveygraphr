#include "surveygraph.h"
#include "unionfind.h"

#define R_NO_REMAP     // TODO comment out for CRAN, only used for debugging
#include <R.h>         // with Rprint.
#include <Rdefines.h>  //

using namespace std;

// Find the largest threshold for which the resulting LCC is greater than or
// equal to the target LCC. Of course, the set of LCCs is degenerate; there can be
// many thresholds that produce identical graphs with LCCs of the same size. We
// choose the largest possible threshold so that the network is as sparse as
// possible.
//
// To do this, simply add edges to the graph, starting with the strongest similarities, 
// until the LCC is greater than or equal to the target. Note that we have to 
// add all edges of a given similarity. As such, we add all edges of a given weight
// at a time. This is in the inner while loop below.
void surveygraph::make_projection_lcc()
{
  // Defines target to be between 1 and survey.size(), inclusive.
  int target = int(ceil(methodval * (survey.size() - 1))) + 1; 

  if(target == 0 || target == 1 || survey.size() == 1){
    edgelist = std::set<edge>{};
    return;
  }

  edgelist_complete();

  unionfind uf(survey.size());

  auto it = edgelist.rbegin();
  double weight = it->weight;  // value of the weight we're currently adding to list
  double oldweight = weight;   // previous weight we added
  int lcc = 1;
  while(lcc < target && it != edgelist.rend()){
    while(it != edgelist.rend() && it->weight == weight){
  	  uf.merge(*it->nodes.begin(), *it->nodes.rbegin());
      ++it;
    }

    oldweight = weight;
    // FIXME without this check, segfaul occurs if we reach rend()
    if(it != edgelist.rend()) weight = it->weight;  
    lcc = uf.lcc;
  }

  edgelist_thresholded(oldweight);
}

void surveygraph::make_projection_avgdegree()
{
  // Number of edges in the the complete graph.
  int ecomplete = survey.size() * (survey.size() - 1) / 2;

  // Defines target to be between 0 and ecomplete, inclusive.
  int target = int(ceil(methodval * ecomplete)); 

  if(target == 0 || survey.size() == 1){
    edgelist = std::set<edge>{};
    return;
  }

  edgelist_complete();

  auto it = edgelist.rbegin();
    
  int e = 0;
  double threshold = it->weight;
  while(e < target && it != edgelist.rend()){
    ++e;
    threshold = it->weight;
    ++it;
  }

  edgelist_thresholded(threshold);
}

void surveygraph::make_projection_similarity()
{
  edgelist_thresholded(methodval);
}
