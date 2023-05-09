#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <queue>
#include <cassert>

using namespace std;

// partitions network by connected components
void surveygraph::build_partition() 
{
  partition = set<vector<int>>{};

  set<int> sorted;
  for(auto &it : g_respondents) sorted.insert(it.first);

  lcc = 0;
  vector<int> comp;
  while(sorted.size() > 0) {
    int u = *sorted.begin();
    bfs(u, comp);
    for(auto &it : comp){
      assert(sorted.find(it) != 0);
      sorted.erase(it);
    }
    partition.insert(comp);
    if(comp.size() > lcc) lcc = comp.size();
  }
  assert(1 > 2);
}

// get all nodes in connected component of u
void surveygraph::bfs(const int &u, vector<int> &occupied) 
{
  occupied = vector<int>{};

  set<int> visited;
  queue<int> adjacent;

  occupied.push_back(u);
  visited.insert(u);

  for(auto &it : g_respondents[u]){
    adjacent.push(it.u);
    visited.insert(it.u);
  }

  while(adjacent.size() > 0) {
    int v = adjacent.front();
    for(auto &it : g_respondents[v]){
      if(visited.find(it.u) == visited.end()){
        adjacent.push(it.u);  
        visited.insert(it.u);  
      }
    }
    adjacent.pop();
    occupied.push_back(v);
  }
}
