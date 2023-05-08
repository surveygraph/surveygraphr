#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

//#include <random>
#include <cmath>

using namespace std;

// pilots the process of constructing respondent and item graphs
void surveygraph::build_pilot()
{
  // builds complete weighted graph (causes memory problems for large m)
  build_g_respondents();
  build_g_items();
}

// build the graph of respondents using Euclidean distance or cosine similarity
void surveygraph::build_g_respondents()
{
  g_respondents = map<int, set<neighbour>> {};

  double threshscale = threshold * 2 * sqrt(n);

  double w = 0.0;
  double wmax = 0.0;
  for(unsigned int i = 0; i < surveyvec.size(); ++i) {
    for(unsigned int j = i + 1; j < surveyvec.size(); ++j) {
      respondent_euclid(int(i), int(j), w);
      if(w < threshscale){
        g_respondents[i].insert(neighbour{int(j), w});
        g_respondents[j].insert(neighbour{int(i), w});
      }
    }
  }
}

// build the graph of items using Euclidean distance or cosine similarity
void surveygraph::build_g_items()
{
  g_items = map<int, set<neighbour>> {};

  double threshscale = threshold * 2 * sqrt(m);

  double w = 0.0;
  for(unsigned int i = 0; i < surveyvec[0].size(); ++i) {
    for(unsigned int j = i + 1; j < surveyvec[0].size(); ++j) {
      item_euclid(int(i), int(j), w);
      if(w < threshscale){
        g_items[i].insert(neighbour{int(j), w});
        g_items[j].insert(neighbour{int(i), w});
      }
    }
  }
}

// Euclidean distance between respondents u and v
void surveygraph::respondent_euclid(const int &u, const int &v, double &w)
{
  w = 0;
  // loop over items
  for(int i = 0; i < n; ++i) {
    double responseu = surveyvec[u][i];
    double responsev = surveyvec[v][i];
    w += pow(responseu - responsev, 2);
  }
  w = sqrt(w);
}

// Euclidean distance between items
void surveygraph::item_euclid(const int &i, const int &j, double &w)
{
  w = 0;
  // loop over respondents
  for(int u = 0; u < m; ++u) {
    double itemi = surveyvec[u][i];
    double itemj = surveyvec[u][j];
    w += pow(itemi - itemj, 2);
  }
  w = sqrt(w);
}
