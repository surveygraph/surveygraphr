#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

// pilots the process of constructing respondent and item graphs
void surveygraph::sweep_thresholds()
{
  threshold_respondents = vector<vector<double>>{};
  threshold_items = vector<vector<double>>{};

  lcc_respondents = 0.95;
  lcc_items = 0.95;

  search_radius_respondents();  // finds optimal radius
  search_radius_items();        // finds optimal radius

  double optimal_radius_respondents = radius_respondents;
  double optimal_radius_items = radius_items;

  int sweep_count = 200;

  double dr_respondents = 1.2 * optimal_radius_respondents / double(sweep_count);
  for(int i = 0; i < sweep_count; ++i){
    radius_respondents = i * dr_respondents;
    build_graph_respondents();
    build_partition_respondents();

    threshold_respondents.push_back(vector<double>{radius_respondents, avg_degree_respondents, double(lcc)});
  }

  double dr_items = 1.2 * optimal_radius_items / double(sweep_count);
  for(int i = 0; i < sweep_count; ++i){
    radius_items = i * dr_items;
    build_graph_items();
    build_partition_items();

    threshold_items.push_back(vector<double>{radius_items, avg_degree_items, double(lcc)});
  }
}
