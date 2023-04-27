#include <iostream>
#include <vector>
#include <random>

#include "surveygraph.h"

/*

To do : change cout etc to printf

*/
void surveygraph::printsurvey()
{
  for(auto it : survey) {
    int u = it.first;

    map<int, int> r = it.second;

    cout << u << " | ";

    for(int j = 0; j < n; ++j) {
      if(r.find(j) != r.end()) {
        cout << r[j] << " ";
      }
      else {
        cout << "NA ";
      }
    }
    cout << endl;
  }
}

void surveygraph::printrespondentgraph()
{
  for(auto &it : G) {
    cout << it.first << " : ";

    for(auto &jt : it.second) {
      cout << jt.u << "[" << jt.w << "] ";
    }
    cout << endl;
  }
}

void surveygraph::printitemgraph()
{
  for(auto &it : H) {
    cout << it.first << " : ";

    for(auto &jt : it.second) {
      cout << jt.u << "[" << jt.w << "] ";
    }
    cout << endl;
  }
}
