#include "surveygraph.h"

#include "R.h"
#include "Rdefines.h"

#include <cmath>

using namespace std;

// build the graph of respondents using Euclidean distance or cosine similarity
void surveygraph::build_graph_respondents()
{
  g_respondents = map<int, set<neighbour>> {};

  double radius_scale = radius_respondents * 2 * sqrt(n);

  double w = 0.0;
  avg_degree_respondents = 0;
  for(unsigned int i = 0; i < survey.size(); ++i) {
    for(unsigned int j = i + 1; j < survey.size(); ++j) {
      distance_respondents(int(i), int(j), w);
      if(w < radius_scale){
        g_respondents[i].insert(neighbour{int(j), w});
        g_respondents[j].insert(neighbour{int(i), w});
        avg_degree_respondents += 2;
      }
    }
  }
  avg_degree_respondents /= double(survey.size());
}

// build the graph of items using Euclidean distance or cosine similarity
void surveygraph::build_graph_items()
{
  g_items = map<int, set<neighbour>> {};

  double radius_scale = radius_items * 2 * sqrt(m);

  double w = 0.0;
  for(unsigned int i = 0; i < survey[0].size(); ++i) {
    for(unsigned int j = i + 1; j < survey[0].size(); ++j) {
      distance_items(int(i), int(j), w);
      if(w < radius_scale){
        g_items[i].insert(neighbour{int(j), w});
        g_items[j].insert(neighbour{int(i), w});
        avg_degree_items += 2;
      }
    }
  }
  avg_degree_items /= double(survey[0].size());
}

// Euclidean distance between respondents u and v
void surveygraph::distance_respondents(const int &u, const int &v, double &w)
{
  w = 0;
  // loop over items
  for(int i = 0; i < n; ++i) {
    double responseu = survey[u][i];
    double responsev = survey[v][i];
    w += pow(responseu - responsev, 2);
  }
  w = sqrt(w);
}

// Euclidean distance between items
void surveygraph::distance_items(const int &i, const int &j, double &w)
{
  w = 0;
  // loop over respondents
  for(int u = 0; u < m; ++u) {
    double itemi = survey[u][i];
    double itemj = survey[u][j];
    w += pow(itemi - itemj, 2);
  }
  w = sqrt(w);
}
