#include "surveygraph.h"

#include <iostream>
#include <random>

using namespace std;

// generates synthetic survey
void surveygraph::build_survey_synthetic()
{
  mt19937 gen(random_device {} ());
  uniform_int_distribution<int> uniform(1, 5);
  uniform_real_distribution<double> real(0, 1);

  survey = map<int, map<int, int>>{};
  surveyvec = vector<vector<double>>(m);

  for(int i = 0; i < m; ++i){
    surveyvec[i] = vector<double>(n);
    for(int j = 0; j < n; ++j) {
      survey[i][j] = uniform(gen);
      surveyvec[i][j] = real(gen);
    }
  }
}
