#include "surveygraph.h"

#include <iostream>
#include <random>
#include <cmath>

using namespace std;

// generates synthetic survey
void surveygraph::buildsynthetic()
{
  mt19937 gen(random_device {} ());
  uniform_int_distribution<int> uniform(1, 5);
  uniform_real_distribution<double> real(0, 1);

  survey = map<int, map<int, int>> {};
  surveyvec = vector<vector<double>> (m);

  for(int i = 0; i < m; ++i) {
    surveyvec[i] = vector<double> (n);

    for(int j = 0; j < n; ++j) {
      survey[i][j] = uniform(gen);
      surveyvec[i][j] = real(gen);
    }
  }
}

// build the graph of respondents using overlap or covariance
void surveygraph::buildrespondentgraph()
{
  G = map<int, set<neighbour>> {};

  int w {0};
  for(unsigned int i = 0; i < surveyvec.size(); ++i) {
    for(unsigned int j = i + 1; j < surveyvec.size(); ++j) {
      respondent_overlap(int(i), int(j), w);
      //if(w > int(n / 2)) {
      if(w > 0) {
        G[i].insert(neighbour {int(j), w});
        G[j].insert(neighbour {int(i), w});
      }
    }
  }
}

// build the graph of items using overlap or covariance
void surveygraph::builditemgraph()
{
  H = map<int, set<neighbour>> {};

  int w {0};

  for(unsigned int i = 0; i < surveyvec[0].size(); ++i) {
    for(unsigned int j = i + 1; j < surveyvec[0].size(); ++j) {
      item_overlap(int(i), int(j), w);
      if(w > int(m / 5)) {
        H[i].insert(neighbour {int(j), w});
        H[j].insert(neighbour {int(i), w});
      }
    }
  }
}

// get response overlap of respondents u and v
// this is the number of responses they have in common
void surveygraph::respondent_overlap(const int &u, const int &v, int &w)
{
  w = 0;
  for(int i = 0; i < n; ++i) {
    // assuming a 1 to 5 scale
    if(abs(surveyvec[u][i] - surveyvec[v][i]) < 0.1) w += 1;
  }
}

// get response overlap of items u and v
void surveygraph::item_overlap(const int &u, const int &v, int &w)
{
  w = 0;
  for(int i = 0; i < m; ++i) {
    if(abs(surveyvec[i][u] - surveyvec[i][v]) < 0.1) w += 1;
  }
}

// Euclidean distance between respondents
void surveygraph::respondent_euclid(const int &u, const int &v, double &c)
{
  likert_1_5 = map<int, map<int, double>> {};
  likert_1_5[1] = map<int, double> {{1, 0.0}, {2, 1.0}, {3, 2.0}, {4, 3.0}, {5, 4.0}};
  likert_1_5[2] = map<int, double> {{1, 1.0}, {2, 0.0}, {3, 1.0}, {4, 2.0}, {5, 3.0}};
  likert_1_5[3] = map<int, double> {{1, 2.0}, {2, 1.0}, {3, 0.0}, {4, 1.0}, {5, 2.0}};
  likert_1_5[4] = map<int, double> {{1, 3.0}, {2, 2.0}, {3, 1.0}, {4, 0.0}, {5, 1.0}};
  likert_1_5[5] = map<int, double> {{1, 4.0}, {2, 3.0}, {3, 2.0}, {4, 1.0}, {5, 0.0}};

  // measure distance between respondents u and v for each of the n items
  for(int i = 0; i < n; ++i) {
    
  }
}

// Euclidean distance between items
void surveygraph::item_euclid(const int &u, const int &v, double &c)
{
}
