#include "surveygraph.h"
#include "unionfind.h"

#define R_NO_REMAP     // TODO comment out for CRAN, only used for debugging 
#include <R.h>         // with Rprint.
#include <Rdefines.h>  //

// Computes the largest connected component, average degree, number of
// connected components, and isolated nodes as a function of the similarity
// threshold. It stores these in the array `profile`.
void surveygraph::make_threshold_profile()
{
  profile = std::vector<std::vector<int>>{};

	edgelist_complete();

	unionfind uf(survey.size());  

  auto it = edgelist.rbegin();
  for(int i = 0; i < count; ++i){
    double threshold = 1 - i / double(count - 1);
    
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
