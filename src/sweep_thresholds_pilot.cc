#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

void surveygraph::sweep_thresholds_agent()
{
  threshold_data_agent = vector<vector<double>>{};

  lcc_agent = 0.95;

  search_threshold_agent_lcc();    // finds optimal threshold

  double optimal_threshold_agent = threshold_agent;

  int sweep_count = 200;

  double dt_agent = 1.2 * optimal_threshold_agent / double(sweep_count);
  for(int i = 0; i < sweep_count; ++i){
    threshold_agent = i * dt_agent;
    build_graph_agent();
    build_partition_agent();

    avg_degree_agent /= double(nrow);
    //assert(avg_degree_agent >= 0 && avg_degree_agent <= 1);
    threshold_data_agent.push_back(vector<double>{threshold_agent, avg_degree_agent, lcc / double(nrow)});
  }
}

// pilots the process of constructing respondent and item graphs
void surveygraph::sweep_thresholds_symbolic()
{
  threshold_data_symbolic = vector<vector<double>>{};

  lcc_symbolic = 0.95;

  search_threshold_symbolic_lcc(); // finds optimal threshold

  double optimal_threshold_symbolic = threshold_symbolic;

  int sweep_count = 200;

  double dt_symbolic = 1.2 * optimal_threshold_symbolic / double(sweep_count);
  for(int i = 0; i < sweep_count; ++i){
    threshold_symbolic = i * dt_symbolic;
    build_graph_symbolic();
    build_partition_symbolic();

    avg_degree_symbolic /= double(ncol);
    //assert(avg_degree_symbolic >= 0 && avg_degree_symbolic <= 1);
    threshold_data_symbolic.push_back(vector<double>{threshold_symbolic, avg_degree_symbolic, lcc / double(ncol)});
  }
}
