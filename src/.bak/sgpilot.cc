#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <random>

using namespace std;

void surveygraph::pilot()
{
  std::mt19937 gen(std::random_device{}());
  std::uniform_real_distribution<double> uniform;

  surveygraph S;
  S.m = 100;
  S.n = 20;

  S.build_survey_synthetic();
  S.build_g_respondents();
}
