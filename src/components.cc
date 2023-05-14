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
void surveygraph::build_partition_respondents() 
{
  partition_respondents = set<vector<int>>{};

  set<int> sorted;
  for(auto &it : g_respondents) sorted.insert(it.first);

  lcc = 0;
  vector<int> comp;
  while(sorted.size() > 0) {
    int u = *sorted.begin();
    bfs_respondents(u, comp);
    for(auto &it : comp){
      assert(sorted.find(it) != 0); // this doesn't output anything inside R
      sorted.erase(it);
    }
    partition_respondents.insert(comp);
    if(comp.size() > lcc) lcc = comp.size();
  }
}

// get all nodes in connected component of u
void surveygraph::bfs_respondents(const int &u, vector<int> &occupied) 
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

// partitions network by connected components
void surveygraph::build_partition_items() 
{
  partition_items = set<vector<int>>{};

  set<int> sorted;
  for(auto &it : g_items) sorted.insert(it.first);

  lcc = 0;
  vector<int> comp;
  while(sorted.size() > 0) {
    int u = *sorted.begin();
    bfs_items(u, comp);
    for(auto &it : comp){
      assert(sorted.find(it) != 0); // this doesn't output anything inside R
      sorted.erase(it);
    }
    partition_items.insert(comp);
    if(comp.size() > lcc) lcc = comp.size();
  }
}

// get all nodes in connected component of u
void surveygraph::bfs_items(const int &u, vector<int> &occupied) 
{
  occupied = vector<int>{};

  set<int> visited;
  queue<int> adjacent;

  occupied.push_back(u);
  visited.insert(u);

  for(auto &it : g_items[u]){
    adjacent.push(it.u);
    visited.insert(it.u);
  }

  while(adjacent.size() > 0) {
    int v = adjacent.front();
    for(auto &it : g_items[v]){
      if(visited.find(it.u) == visited.end()){
        adjacent.push(it.u);  
        visited.insert(it.u);  
      }
    }
    adjacent.pop();
    occupied.push_back(v);
  }
}
