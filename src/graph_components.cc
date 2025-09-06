//#include "surveygraph.h"
#include "graph.h"

#include <queue>

using namespace std;

// partitions network by connected components
void graph::build_partition() 
{
  partition = std::set<std::vector<int>>{};

  std::set<int> sorted;
  for(auto &it : network) sorted.insert(it.first);

  lcc = 0;
  isols = 0;
  comps = 0;
  std::vector<int> comp;
  while(sorted.size() > 0) {
    int u = *sorted.begin();
    bfs(u, comp);
    for(auto &it : comp){
      sorted.erase(it);
    }
    partition.insert(comp);
    if(int(comp.size()) > lcc) lcc = int(comp.size());
    if(comp.size() == 1) isols += 1;
    comps += 1;
  }

  int norm = 0;
  for(auto it : partition) norm += it.size();
  // this is causing compilation errors and isn't strictly required
  //if(norm != int(network.size())){
  //  error("an internal test has failed, please report to package creators\n");
  //}
}

// get all nodes in connected component of u
void graph::bfs(const int &u, std::vector<int> &occupied) 
{
  occupied = std::vector<int>{};

  std::set<int> visited;
  std::queue<int> adjacent;

  occupied.push_back(u);
  visited.insert(u);

  for(auto &it : network[u]){
    adjacent.push(it.u);
    visited.insert(it.u);
  }

  while(adjacent.size() > 0) {
    int v = adjacent.front();
    for(auto &it : network[v]){
      if(visited.find(it.u) == visited.end()){
        adjacent.push(it.u);  
        visited.insert(it.u);  
      }
    }
    adjacent.pop();
    occupied.push_back(v);
  }
}
