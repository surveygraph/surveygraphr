#include "surveygraph.h"

#include "R.h"
#include "Rdefines.h"

#include <cmath>

using namespace std;

// build the graph of respondents using Euclidean distance or cosine similarity
void surveygraph::build_graph_respondents()
{
  g_respondents = map<int, set<neighbour>>{};

  double radius_scale = radius_respondents * 2 * sqrt(ncol);

  double w = 0.0;
  e_respondents = 0;
  avg_degree_respondents = 0;
  for(unsigned int i = 0; i < survey.size(); ++i){
    for(unsigned int j = i + 1; j < survey.size(); ++j){
      man_distance_respondents(int(i), int(j), w);
      if(w < radius_scale){
        g_respondents[i].insert(neighbour{int(j), w});
        g_respondents[j].insert(neighbour{int(i), w});
        avg_degree_respondents += 2;
        e_respondents += 1;
      }
    }
  }
  avg_degree_respondents /= double(survey.size());
}

// build the graph of items using Euclidean distance or cosine similarity
void surveygraph::build_graph_items()
{
  g_items = map<int, set<neighbour>>{};

  double radius_scale = radius_items * 2 * sqrt(nrow);

  double w = 0.0;
  e_items = 0;
  avg_degree_items = 0;
  for(unsigned int i = 0; i < survey[0].size(); ++i){
    for(unsigned int j = i + 1; j < survey[0].size(); ++j){
      man_distance_items(int(i), int(j), w);
      if(w < radius_scale){
        g_items[i].insert(neighbour{int(j), w});
        g_items[j].insert(neighbour{int(i), w});
        avg_degree_items += 2;
        e_items += 1;
      }
    }
  }
  avg_degree_items /= double(survey[0].size());
}

// Manhattan distance between respondents u and v
void surveygraph::man_distance_respondents(const int &u, const int &v, double &w)
{
  w = 0;
  // loop over items
  for(int i = 0; i < ncol; ++i){
    double responseu = survey[u][i];
    double responsev = survey[v][i];
    w += abs(responseu - responsev);
  }
  //w -= ncol;
}

// Manhattan distance between items
void surveygraph::man_distance_items(const int &i, const int &j, double &w)
{
  w = 0;
  // loop over respondents
  for(int u = 0; u < nrow; ++u){
    double itemi = survey[u][i];
    double itemj = survey[u][j];
    w += abs(itemi - itemj);
  }
  //w -= nrow;
}

// Euclidean distance between respondents u and v
void surveygraph::euclid_distance_respondents(const int &u, const int &v, double &w)
{
  w = 0;
  // loop over items
  for(int i = 0; i < ncol; ++i){
    double responseu = survey[u][i];
    double responsev = survey[v][i];
    w += pow(responseu - responsev, 2);
  }
  w = sqrt(w);
}

// Euclidean distance between items
void surveygraph::euclid_distance_items(const int &i, const int &j, double &w)
{
  w = 0;
  // loop over respondents
  for(int u = 0; u < nrow; ++u){
    double itemi = survey[u][i];
    double itemj = survey[u][j];
    w += pow(itemi - itemj, 2);
  }
  w = sqrt(w);
}
