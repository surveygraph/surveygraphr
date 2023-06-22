#include "surveygraph.h"
#include "graph.h"

#include "R.h"
#include "Rdefines.h"

#include <cmath>

using namespace std;

//// build the graph of symbolic using Euclidean distance or cosine similarity
//void surveygraph::build_graph_symbolic()
//{
//  g_symbolic = map<int, set<neighbour>>{};
//
//  for(int i = 0; i < ncol; ++i) g_symbolic[i] = set<neighbour>{};
//
//  double w = 0.0;
//  e_symbolic = 0;
//  avg_degree_symbolic = 0;
//  for(unsigned int i = 0; i < survey[0].size(); ++i){
//    for(unsigned int j = i + 1; j < survey[0].size(); ++j){
//      man_distance_symbolic(int(i), int(j), w);
//      if(w > threshold_symbolic){
//        g_symbolic[i].insert(neighbour{int(j), w});
//        g_symbolic[j].insert(neighbour{int(i), w});
//        avg_degree_symbolic += 2;
//        e_symbolic += 1;
//      }
//    }
//  }
//  avg_degree_symbolic /= double(ncol);
//}

//// build the agent layer graph using Euclidean distance or cosine similarity
//void surveygraph::build_graph_agent()
//{
//  g_agent = map<int, set<neighbour>>{};
//
//  for(int i = 0; i < nrow; ++i) g_agent[i] = set<neighbour>{};
//
//  e_agent = 0;
//  avg_degree_agent = 0;
//  for(unsigned int i = 0; i < survey.size(); ++i){
//    for(unsigned int j = i + 1; j < survey.size(); ++j){
//      double w = 0.0;
//      man_distance_agent(int(i), int(j), w);
//      if(w > threshold_agent){
//        g_agent[i].insert(neighbour{int(j), w});
//        g_agent[j].insert(neighbour{int(i), w});
//        avg_degree_agent += 2;
//        e_agent += 1;
//      }
//    }
//  }
//  avg_degree_agent /= double(nrow);
//}

// build graph using Euclidean distance or cosine similarity
void graph::build_graph(const surveydef &S)
{
  network = map<int, set<neighbour>>{};
  for(int i = 0; i < n; ++i) network[i] = set<neighbour>{};

  e = 0;
  avg_degree = 0;
  for(unsigned int i = 0; i < n; ++i){
    for(unsigned int j = i + 1; j < n; ++j){
      double w = 0.0;
      man_distance(S, int(i), int(j), w);
      if(w > threshold){
        network[i].insert(neighbour{int(j), w});
        network[j].insert(neighbour{int(i), w});
        avg_degree += 2;
        e += 1;
      }
    }
  }
  avg_degree /= double(n);
}

// Manhattan distance between rows or columns u and v
void graph::man_distance(const surveydef &S, const int &u, const int &v, double &w)
{
  switch(layer){
    case Layer::agent:
      w = 0;
      for(int i = 0; i < m; ++i)
        w += abs(S[u][i] - S[v][i]);
      w = (double(m) - w) / double(m);
      break;
    case Layer::symbolic:
      w = 0;
      for(int i = 0; i < m; ++i)
        w += abs(S[i][u] - S[i][v]);
      w = (double(m) - w) / double(m);
      break;
  }
}

//// Manhattan distance between agents u and v
//void surveygraph::man_distance_agent(const int &u, const int &v, double &w)
//{
//  w = 0;
//  // loop over columns
//  for(int i = 0; i < ncol; ++i){
//    double responseu = survey[u][i];
//    double responsev = survey[v][i];
//    w += abs(responseu - responsev);
//  }
//  w = (double(ncol) - w) / double(ncol);
//}
//
//// Manhattan distance between symbolic i and j
//void surveygraph::man_distance_symbolic(const int &i, const int &j, double &w)
//{
//  w = 0;
//  // loop over rows
//  for(int u = 0; u < nrow; ++u){
//    double itemi = survey[u][i];
//    double itemj = survey[u][j];
//    w += abs(itemi - itemj);
//  }
//  w = (double(nrow) - w) / double(nrow);
//}
//
//// Euclidean distance between agents u and v
//void surveygraph::euclid_distance_agent(const int &u, const int &v, double &w)
//{
//  w = 0;
//  // loop over columns
//  for(int i = 0; i < ncol; ++i){
//    double responseu = survey[u][i];
//    double responsev = survey[v][i];
//    w += pow(responseu - responsev, 2);
//  }
//  w = sqrt(w);
//}
//
//// Euclidean distance between symbolic
//void surveygraph::euclid_distance_symbolic(const int &i, const int &j, double &w)
//{
//  w = 0;
//  // loop over rows
//  for(int u = 0; u < nrow; ++u){
//    double itemi = survey[u][i];
//    double itemj = survey[u][j];
//    w += pow(itemi - itemj, 2);
//  }
//  w = sqrt(w);
//}
