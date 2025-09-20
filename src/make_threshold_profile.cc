#include "surveygraph.h"

#define R_NO_REMAP     // TODO comment out for CRAN, only used for debugging 
#include <R.h>         // with Rprint.
#include <Rdefines.h>  //

using namespace std;

class UF 
{
  int *id, cnt, ecnt, isols, lcc, *sz;

  public:
		// Create an empty union find data structure with N isolated sets.
		UF(int N){
			cnt = N; 
			lcc = 1;
			ecnt = 0;
			isols = N;
			id = new int[N];
			sz = new int[N];
			for(int i = 0; i < N; i++){
				id[i] = i;
				sz[i] = 1; 
			}
		}

		~UF(){ 
			delete[] id; 
			delete[] sz; 
		}

		// Return the id of component corresponding to object p.
		int find(int p){
			int root = p;
			while(root != id[root]) root = id[root];
			while(p != root){
				int newp = id[p]; 
				id[p] = root; 
				p = newp;
			}
			return root;
		}

		// Replace sets containing x and y with their union.
		void merge(int x, int y){
			int i = find(x); 
			int j = find(y); 
			++ecnt;
			if(i == j) return;

			if(sz[i] == 1) isols--;
			if(sz[j] == 1) isols--;

			// make smaller root point to larger one
			if(sz[i] < sz[j]){
				id[i] = j;
				sz[j] += sz[i]; 
				if(sz[j] > lcc) lcc = sz[j];
			}else{
				id[j] = i;
				sz[i] += sz[j];
				if(sz[i] > lcc) lcc = sz[i];
			}
			cnt--;
		}

		// Are objects x and y in the same set?
		bool connected(int x, int y){return find(x) == find(y);}

		// Return the number of disjoint sets.
		int count(){return cnt;}

		// Return the number of edges.
		int ecount(){return ecnt;}

		// Return the number of singleton sets.
		int isolscount(){return isols;}

		// Return the size of the largest set.
		int largest(){return lcc;}
};

// Implements union find algorithm, with slight adaptations.
void surveygraph::make_threshold_profile()
{
  profile = std::vector<std::vector<int>>{};

  g = graph(mincomps, metric, survey); 

	UF uf(survey.size());  

  auto it = g.edgelist.rbegin();
  for(int i = 0; i < count; ++i){
    double threshold = 1 - i / double(count - 1);
    
    // add edges to graph, merging components
    while(it->weight >= threshold && it != g.edgelist.rend()){
			uf.merge(*it->nodes.begin(), *it->nodes.rbegin());
      ++it;
    }

    std::vector<int> dummy;
    dummy.push_back(uf.largest());
    dummy.push_back(uf.ecount());
    dummy.push_back(uf.count());
    dummy.push_back(uf.isolscount());
    profile.push_back(dummy);
  }
}
