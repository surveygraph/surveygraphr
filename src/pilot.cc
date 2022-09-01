#include "surveygraph.h"

#include <random>

using namespace std;

void surveygraph::pilot()
{
  std::mt19937 gen(std::random_device {} ());
  std::uniform_real_distribution<double> uniform;

  surveygraph S;
  S.m = 100;
  S.n = 20;

  S.buildsynthetic();
  S.buildrespondentgraph();
  S.printrespondentgraph();
}

void surveygraph::dummy(const int &m, const int &n)
{
  std::mt19937 gen(std::random_device {} ());
  std::uniform_real_distribution<double> uniform;

  surveygraph S;
  S.m = m;
  S.n = n;

  S.buildsynthetic();
  S.buildrespondentgraph();
  S.buildquestiongraph();

  S.writesurvey();
  S.writequestiongraph();
  S.writerespondentgraph();
}
