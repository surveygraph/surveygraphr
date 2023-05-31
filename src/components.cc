#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <queue>
#include <cassert>

using namespace std;

/*

currently only does respondent network, need to generalise

*/

// partitions network by connected components
void surveygraph::build_partition_agent() 
{
  partition_agent = set<vector<int>>{};

  set<int> sorted;
  for(auto &it : g_agent) sorted.insert(it.first);

  lcc = 0;
  vector<int> comp;
  while(sorted.size() > 0) {
    int u = *sorted.begin();
    bfs_agent(u, comp);
    for(auto &it : comp){
      assert(sorted.find(it) != 0); // this doesn't output anything inside R
      sorted.erase(it);
    }
    partition_agent.insert(comp);
    if(comp.size() > lcc) lcc = comp.size();
  }
}

// get all nodes in connected component of u
void surveygraph::bfs_agent(const int &u, vector<int> &occupied) 
{
  occupied = vector<int>{};

  set<int> visited;
  queue<int> adjacent;

  occupied.push_back(u);
  visited.insert(u);

  for(auto &it : g_agent[u]){
    adjacent.push(it.u);
    visited.insert(it.u);
  }

  while(adjacent.size() > 0) {
    int v = adjacent.front();
    for(auto &it : g_agent[v]){
      if(visited.find(it.u) == visited.end()){
        adjacent.push(it.u);  
        visited.insert(it.u);  
      }
    }
    adjacent.pop();
    occupied.push_back(v);
  }
}

// partitions network by connected components
void surveygraph::build_partition_symbolic() 
{
  partition_symbolic = set<vector<int>>{};

  set<int> sorted;
  for(auto &it : g_symbolic) sorted.insert(it.first);

  lcc = 0;
  vector<int> comp;
  while(sorted.size() > 0) {
    int u = *sorted.begin();
    bfs_symbolic(u, comp);
    for(auto &it : comp){
      assert(sorted.find(it) != 0); // this doesn't output anything inside R
      sorted.erase(it);
    }
    partition_symbolic.insert(comp);
    if(comp.size() > lcc) lcc = comp.size();
  }
}

// get all nodes in connected component of u
void surveygraph::bfs_symbolic(const int &u, vector<int> &occupied) 
{
  occupied = vector<int>{};

  set<int> visited;
  queue<int> adjacent;

  occupied.push_back(u);
  visited.insert(u);

  for(auto &it : g_symbolic[u]){
    adjacent.push(it.u);
    visited.insert(it.u);
  }

  while(adjacent.size() > 0) {
    int v = adjacent.front();
    for(auto &it : g_symbolic[v]){
      if(visited.find(it.u) == visited.end()){
        adjacent.push(it.u);  
        visited.insert(it.u);  
      }
    }
    adjacent.pop();
    occupied.push_back(v);
  }
}
