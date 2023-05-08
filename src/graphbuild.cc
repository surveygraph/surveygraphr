#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

//#include <iostream>
#include <random>
#include <cmath>

using namespace std;

void surveygraph::sparse_g_respondents()
{
  //for(auto &it : g_respondents) {
  //  Rprintf("%d : ", it.first);
  //  for(auto &jt : it.second) {
  //    Rprintf("%d [%f] ", jt.u, jt.w);
  //  }
  //  Rprintf("\n");
  //}
}

void surveygraph::sparse_g_items()
{
  for(auto &it : g_items) {
    for(auto &jt : it.second) {
      // TO DO
    }
  }
}

// pilots the process of constructing respondent and item graphs
void surveygraph::build_pilot()
{
  // build metrics for Likert scale with five levels
  likert_1_5 = map<int, map<int, double>> {};
  likert_1_5[1] = map<int, double> {{1, 0.0}, {2, 1.0}, {3, 2.0}, {4, 3.0}, {5, 4.0}};
  likert_1_5[2] = map<int, double> {{1, 1.0}, {2, 0.0}, {3, 1.0}, {4, 2.0}, {5, 3.0}};
  likert_1_5[3] = map<int, double> {{1, 2.0}, {2, 1.0}, {3, 0.0}, {4, 1.0}, {5, 2.0}};
  likert_1_5[4] = map<int, double> {{1, 3.0}, {2, 2.0}, {3, 1.0}, {4, 0.0}, {5, 1.0}};
  likert_1_5[5] = map<int, double> {{1, 4.0}, {2, 3.0}, {3, 2.0}, {4, 1.0}, {5, 0.0}};

  // build metrics for Likert scale with four levels
  // TO DO
  // build metrics for Likert scale with three levels
  // TO DO

  // builds complete weighted graph (causes memory problems for large m)
  build_g_respondents();
  build_g_items();

  // threshold search, what is your criteria? fully connected?
  sparse_g_respondents();
  sparse_g_items();
}

// build the graph of respondents using Euclidean distance or cosine similarity
void surveygraph::build_g_respondents()
{
  g_respondents = map<int, set<neighbour>> {};

  double w {0.0};
  for(unsigned int i = 0; i < surveyvec.size(); ++i) {
    for(unsigned int j = i + 1; j < surveyvec.size(); ++j) {
      respondent_euclid(int(i), int(j), w);
      g_respondents[i].insert(neighbour{int(j), w});
      g_respondents[j].insert(neighbour{int(i), w});
    }
  }
}

// build the graph of items using Euclidean distance or cosine similarity
void surveygraph::build_g_items()
{
  g_items = map<int, set<neighbour>> {};

  double w {0.0};
  for(unsigned int i = 0; i < surveyvec[0].size(); ++i) {
    for(unsigned int j = i + 1; j < surveyvec[0].size(); ++j) {
      //item_overlap(int(i), int(j), w);
      item_euclid(int(i), int(j), w);
      g_items[i].insert(neighbour{int(j), w});
      g_items[j].insert(neighbour{int(i), w});
    }
  }
}

// Euclidean distance between respondents u and v
void surveygraph::respondent_euclid(const int &u, const int &v, double &w)
{
  w = 0;
  // loop over items
  for(int i = 0; i < n; ++i) {
    int responseu = surveyvec[u][i];
    int responsev = surveyvec[v][i];
    w += pow(likert_1_5[responseu][responsev], 2);
  }
  w = sqrt(w);
}

// Euclidean distance between items
void surveygraph::item_euclid(const int &i, const int &j, double &w)
{
  w = 0;
  // loop over respondents
  for(int u = 0; u < m; ++u) {
    int itemi = surveyvec[u][i];
    int itemj = surveyvec[u][j];
    w += pow(likert_1_5[itemi][itemj], 2);
  }
  w = sqrt(w);
}

// overlap, or number of common responses for respondents u and v
void surveygraph::respondent_overlap(const int &u, const int &v, double &w)
{
  w = 0;
  for(int i = 0; i < n; ++i) {
    if(abs(surveyvec[u][i] - surveyvec[v][i]) < 0.01) w += 1;
  }
}

// overlap, or number of common responses for items u and v
void surveygraph::item_overlap(const int &u, const int &v, double &w)
{
  w = 0;
  for(int i = 0; i < m; ++i) {
    if(abs(surveyvec[i][u] - surveyvec[i][v]) < 0.1) w += 1;
  }
}
