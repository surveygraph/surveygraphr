#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

// pilots the process of constructing respondent and item graphs
void surveygraph::explore_pilot()
{
  explore_respondents = vector<vector<double>>{};

  // sweep through a range of radii and record statistics
  // currently tailored to a specific type of survey, namely 1000 x 25 survey
  // containing entries in the range (1, 10).
  for(double r = 0; r < 0.5; r += 0.001){
    radius_respondents = r;
    build_graph_respondents();
    build_partition();
    explore_respondents.push_back(vector<double>{radius_respondents, avg_degree_respondents, double(lcc)});
  }
}
