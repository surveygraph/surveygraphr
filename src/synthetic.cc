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
      respondentoverlap(int(i), int(j), w);
  //    if(w > int(n / 2)) {
        G[i].insert(neighbour {int(j), w});
        G[j].insert(neighbour {int(i), w});
   //   }
    }
  }
}

// build the graph of questions using overlap or covariance
void surveygraph::buildquestiongraph()
{
  H = map<int, set<neighbour>> {};

  int w {0};

  for(unsigned int i = 0; i < surveyvec[0].size(); ++i) {
    for(unsigned int j = i + 1; j < surveyvec[0].size(); ++j) {
      questionoverlap(int(i), int(j), w);
      if(w > int(m / 5)) {
        H[i].insert(neighbour {int(j), w});
        H[j].insert(neighbour {int(i), w});
      }
    }
  }
}

// get response overlap of respondents u and v
// this is the number of responses they have in common
void surveygraph::respondentoverlap(const int &u, const int &v, int &w)
{
  w = 0;
  for(int i = 0; i < n; ++i) {
    // assuming a 1 to 5 scale
    if(abs(surveyvec[u][i] - surveyvec[v][i]) < 0.1) w += 1;
  }
}

// get response overlap of questions u and v
void surveygraph::questionoverlap(const int &u, const int &v, int &w)
{
  w = 0;
  for(int i = 0; i < m; ++i) {
    // assuming a 1 to 5 scale
    if(abs(surveyvec[i][u] - surveyvec[i][v]) < 0.1) w += 1;
  }
}

void surveygraph::respondentcov(const int &u, const int &v, double &c)
{
}

void surveygraph::questioncov(const int &u, const int &v, double &c)
{
}
