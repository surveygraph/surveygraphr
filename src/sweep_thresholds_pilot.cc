#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

// pilots the process of constructing respondent and item graphs
void surveygraph::sweep_thresholds_pilot()
{
  threshold_respondents = vector<vector<double>>{};

  search_radius_respondents();  // finds optimal radius

  double optimal_radius_respondents = radius_respondents;

  for(double r = 0; r < 1.2 * optimal_radius_respondents; r += 0.001){
    radius_respondents = r;
    build_graph_respondents();
    build_partition_respondents();

    threshold_respondents.push_back(vector<double>{radius_respondents, avg_degree_respondents, double(lcc)});
    Rprintf("sweep data: %f %d\n", radius_respondents);
  }
}
