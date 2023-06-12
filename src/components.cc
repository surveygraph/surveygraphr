#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <queue>
//#include <cassert>

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

  //if(sorted.size() != g_agent.size()) Rprintf("you fuuuucked uuuuuuup\n");

  lcc = 0;
  isol_agent = 0;
  comp_agent = 0;
  while(sorted.size() > 0){
    int u = *sorted.begin();
    vector<int> comp;
    bfs_agent(u, comp);
    for(auto &it : comp){
      sorted.erase(it);
    }
    partition_agent.insert(comp);
    if(comp.size() > lcc) lcc = comp.size();
    if(comp.size() == 1) isol_agent += 1;
    comp_agent += 1;
  }

  int norm = 0;
  for(auto it : partition_agent) norm += it.size();
  if(norm != g_agent.size()){
    error("an internal test has failed, please report to package creators\n");
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
  isol_symbolic = 0;
  comp_symbolic = 0;
  vector<int> comp;
  while(sorted.size() > 0) {
    int u = *sorted.begin();
    bfs_symbolic(u, comp);
    for(auto &it : comp){
      sorted.erase(it);
    }
    partition_symbolic.insert(comp);
    if(comp.size() > lcc) lcc = comp.size();
    if(comp.size() == 1) isol_symbolic += 1;
    comp_symbolic += 1;
  }
  int norm = 0;
  for(auto it : partition_symbolic) norm += it.size();
  if(norm != g_symbolic.size()){
    error("an internal test has failed, please report to package creators\n");
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
