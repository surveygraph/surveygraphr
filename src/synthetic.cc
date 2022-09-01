#include "surveygraph.h"

#include <iostream>
#include <random>

using namespace std;

// generates synthetic survey
void surveygraph::buildsynthetic()
{
  mt19937 gen(random_device {} ());
  uniform_int_distribution<int> uniform(1, 5);
  uniform_real_distribution<double> real(0, 1);

  survey = map<int, map<int, int>> {};
  surveyvec = vector<vector<int>> (m);

  for(int i = 0; i < m; ++i) {
    surveyvec[i] = vector<int> (n);

    for(int j = 0; j < n; ++j) {
      survey[i][j] = uniform(gen);
      surveyvec[i][j] = uniform(gen);
    }
  }
}

// build the graph of respondents using overlap or covariance
void surveygraph::buildrespondentgraph()
{
  G = map<int, set<neighbour>> {};

  int w {0};
  //int E {0};
  //double c {0};
  //map<int, int> hist;

  for(unsigned int i = 0; i < surveyvec.size(); ++i) {
    for(unsigned int j = i + 1; j < surveyvec.size(); ++j) {
      respondentoverlap(int(i), int(j), w);
      //hist[w] += 1;
      if(w > int(n / 2)) {
        G[i].insert(neighbour {int(j), w});
        G[j].insert(neighbour {int(i), w});
        //E += 1;
      }
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
void surveygraph::respondentoverlap(const int &u, const int &v, int &w)
{
  w = 0;
  for(int i = 0; i < n; ++i) {
    if(surveyvec[u][i] == surveyvec[v][i]) w += 1;
  }
}

// get response overlap of questions u and v
void surveygraph::questionoverlap(const int &u, const int &v, int &w)
{
  w = 0;
  for(int i = 0; i < m; ++i) {
    if(surveyvec[i][u] == surveyvec[i][v]) w += 1;
  }
}

void surveygraph::respondentcov(const int &u, const int &v, double &c)
{
}

void surveygraph::questioncov(const int &u, const int &v, double &c)
{
}
