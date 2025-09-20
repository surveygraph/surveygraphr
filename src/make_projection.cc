#include "surveygraph.h"

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
// Note that we could take advantage of the fact that in small graphs, the
// set of edge weights, or similarities, will be small, meaning we can avoid
// unnecessary halving-the-interval steps below. However, since small graphs 
// are fast to compute anyway, we don't bother.
//void surveygraph::search_threshold_lcc()
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

void surveygraph::make_projection_lcc()
{
  // ensures target is in range [1, N], while methodval is in range [0, 1]
  //int target = int(ceil(target_lcc * (survey.size() - 1))) + 1; 
  int target = int(ceil(methodval * (survey.size() - 1))) + 1; 

  if(target == 0 || target == 1 || survey.size() == 1){
    g = graph(1.0001, mincomps, metric, survey); 
    return;
  }

  g = graph(mincomps, metric, survey); // edgelist of complete graph

  UF uf(survey.size());
    
  auto it = g.edgelist.rbegin();
  double current = it->weight;
  double old = current;

  int lcc = 1;
  while(lcc < target && it != g.edgelist.rend()){

    // add the next chunk of edges, merging components
    while(it->weight == current && it != g.edgelist.rend()){
			uf.merge(*it->nodes.begin(), *it->nodes.rbegin());
      ++it;
    }

    old = current;
    current = it->weight;
    lcc = uf.largest();
  }

  g = graph(old, mincomps, metric, survey); 
}

//void surveygraph::make_projection_lcc()
//{
//  // Recall that LCC is non-increasing with the similarity threshold.
//  double thresholdlower = -0.001;  // LCC guaranteed to be survey.size()
//  double thresholdupper =  1.001;  // LCC guaranteed to be 1
//  int lcclower = survey.size();    // we ensure lcclower >= target LCC    
//  int lccupper = 1;                // we ensure lccupper < target LCC
//
//  // ensures target is in range [1, N], while methodval is in range [0, 1]
//  //int target = int(ceil(target_lcc * (survey.size() - 1))) + 1; 
//  int target = int(ceil(methodval * (survey.size() - 1))) + 1; 
//
//  if(target == 0 || target == 1 || survey.size() == 1){
//    g = graph(1.0001, mincomps, metric, survey); 
//    return;
//  }
//
//  int count = 0;
//  while(count < 10){
//    double threshold = (thresholdlower + thresholdupper) / 2.0;
//
//    g = graph(threshold, mincomps, metric, survey); 
//    
//    if(g.lcc >= target)
//      thresholdlower = threshold;
//    else
//      thresholdupper = threshold;
//
//    count += 1;
//  }
//  g = graph(thresholdlower, mincomps, metric, survey); 
//}

void surveygraph::make_projection_avgdegree()
{
  int emax = survey.size() * (survey.size() - 1) / 2;

  double eps = 1e-3;

  int target = int(ceil(methodval * emax)); 

  if(target == 0 || survey.size() == 1){
    g = graph(1.0001, mincomps, metric, survey); 
    return;
  }

  g = graph(mincomps, metric, survey); // make edgelist of complete graph

  auto it = g.edgelist.rbegin();
    
  int e = 0;
  double threshold = it->weight;
  while(e < target && it != g.edgelist.rend()){
    ++e;
    threshold = it->weight;
    ++it;
  }

  g = graph(threshold, mincomps, metric, survey); 
}

void surveygraph::make_projection_similarity()
{
  //double threshold = raw_similarity;
  double threshold = methodval;
  g = graph(threshold, mincomps, metric, survey); 
}
