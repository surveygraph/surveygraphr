#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

/*

THIS NEEDS WAY MORE TESTING

*/
void surveygraph::make_threshold_profile_agent()
{
  threshold_data_agent = vector<vector<double>>{};

  target_lcc = 1.00;
  search_threshold_agent_lcc();    // finds optimal threshold

  double optimal_threshold_agent = threshold_agent;

  int count = 20;

  //double dt_agent = 1.5 * optimal_threshold_agent / double(count);
  double dt_agent = 1.0 / double(count);
  for(int i = 0; i < count; ++i){
    threshold_agent = i * dt_agent;
    build_graph_agent();
    build_partition_agent();

    avg_degree_agent /= double(nrow);
    vector<double> dummy;
    dummy.push_back(threshold_agent);
    dummy.push_back(avg_degree_agent);
    dummy.push_back(lcc / double(nrow));
    dummy.push_back(double(isol_agent));
    dummy.push_back(double(comp_agent));
    threshold_data_agent.push_back(dummy);

    if(!(avg_degree_agent >= 0 && avg_degree_agent <= 1)){
      error("an internal test has failed, please report to package creators\n");
    }
  }
}

// pilots the process of constructing respondent and item graphs
void surveygraph::make_threshold_profile_symbolic()
{
  threshold_data_symbolic = vector<vector<double>>{};

  target_lcc = 0.95;
  search_threshold_symbolic_lcc(); // sets threshold_symbolic to optimal threshold

  double optimal_threshold_symbolic = threshold_symbolic;

  int count = 250;

  //double dt_symbolic = 1.5 * optimal_threshold_symbolic / double(count);
  double dt_symbolic = 1.0 / double(count);
  for(int i = 0; i < count; ++i){
    threshold_symbolic = i * dt_symbolic;
    build_graph_symbolic();
    build_partition_symbolic();

    avg_degree_symbolic /= double(ncol);
    vector<double> dummy;
    dummy.push_back(threshold_symbolic);
    dummy.push_back(avg_degree_symbolic);
    dummy.push_back(lcc / double(ncol));
    dummy.push_back(double(isol_symbolic));
    dummy.push_back(double(comp_symbolic));
    threshold_data_symbolic.push_back(dummy);

    if(!(avg_degree_symbolic >= 0 && avg_degree_symbolic <= 1)){
      error("an internal test has failed, please report to package creators\n");
    }
  }
}
