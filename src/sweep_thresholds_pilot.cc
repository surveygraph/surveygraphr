#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

// pilots the process of constructing respondent and item graphs
void surveygraph::sweep_thresholds_pilot()
{
  threshold_respondents = vector<vector<double>>{};
  threshold_items = vector<vector<double>>{};

  lcc_respondents = 0.95;
  lcc_items = 0.95;

  search_radius_respondents();  // finds optimal radius
  search_radius_items();        // finds optimal radius

  double optimal_radius_respondents = radius_respondents;
  double optimal_radius_items = radius_items;

  Rprintf("optimal radius respondents: %f\n", optimal_radius_respondents);
  Rprintf("optimal radius items: %f\n", optimal_radius_items);

  for(double r = 0; r < 1.2 * optimal_radius_respondents; r += 0.1){
    radius_respondents = r;
    build_graph_respondents();
    build_partition_respondents();

    threshold_respondents.push_back(vector<double>{radius_respondents, avg_degree_respondents, double(lcc)});
    Rprintf("sweep data: %f %d\n", radius_respondents, lcc);
  }

  for(double r = 0; r < 1.2 * optimal_radius_items; r += 0.1){
    radius_items = r;
    build_graph_items();
    build_partition_items();

    threshold_items.push_back(vector<double>{radius_items, avg_degree_items, double(lcc)});
    Rprintf("sweep data: %f %d\n", radius_items, lcc);
  }
}
