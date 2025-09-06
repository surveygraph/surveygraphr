#include "surveygraph.h"

//#define R_NO_REMAP
//#include "R.h"
//#include "Rdefines.h"

using namespace std;

void surveygraph::make_threshold_profile()
{
  profile = std::vector<std::vector<double>>{};

  target_lcc = 1.00;
  search_threshold_lcc();    // finds optimal threshold

  //double optimal_threshold = g.threshold;

  int count = 200;

  double dt = 1.0 / double(count);
  for(int i = 0; i < count; ++i){
    double threshold = i * dt;

    g = graph(0, threshold, mincomps, survey);

    std::vector<double> dummy;
    dummy.push_back(threshold);
    dummy.push_back(g.avg_degree / double(g.network.size()));
    dummy.push_back(g.lcc / double(g.network.size()));
    dummy.push_back(double(g.isols));
    dummy.push_back(double(g.comps));

    profile.push_back(dummy);

    //if(!(g.avg_degree / double(g.n) >= 0 && g.avg_degree / double(g.n) <= 1)){
    //  error("an internal test has failed, please report to package creators\n");
    //}
  }
}
