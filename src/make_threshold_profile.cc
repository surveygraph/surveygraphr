#include "surveygraph.h"
#include "uf.h"

#define R_NO_REMAP     // TODO comment out for CRAN, only used for debugging 
#include <R.h>         // with Rprint.
#include <Rdefines.h>  //

using namespace std;

// Implements union find algorithm, with slight adaptations.
void surveygraph::make_threshold_profile()
{
	// TODO tests for surveys with a single entry.. how to treat profile?
  //if(survey.size() == 1){
  //  edgelist = std::set<edge>{};
  //  return;
  //}

  profile = std::vector<std::vector<int>>{};

  //g = graph(mincomps, metric, survey); 
	edgelist_complete();

	UF uf(survey.size());  

  //auto it = g.edgelist.rbegin();
  auto it = edgelist.rbegin();
  for(int i = 0; i < count; ++i){
    double threshold = 1 - i / double(count - 1);
    
    //while(it->weight >= threshold && it != g.edgelist.rend()){
    while(it->weight >= threshold && it != edgelist.rend()){
			uf.merge(*it->nodes.begin(), *it->nodes.rbegin());
      ++it;
    }

    std::vector<int> dummy;
    dummy.push_back(uf.lcc);
    dummy.push_back(uf.e);
    dummy.push_back(uf.comps);
    dummy.push_back(uf.isolated);
    profile.push_back(dummy);
  }
}
