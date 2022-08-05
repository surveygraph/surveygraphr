#include <Rcpp.h>

#include "sgraph.h"

#include <random>

using namespace Rcpp;

// [[Rcpp::export]]
void ToySurvey()
{
  sgraph sg;
  sg.fillrandom();
}

// [[Rcpp::export]]
void ToySurveyArgNV (NumericVector &v)
{
  std::cout << "before: " << std::endl;
  for(auto &it : v) std::cout << it << std::endl;
  for(auto &it : v) it *= 2;
  std::cout << "after: " << std::endl;
  for(auto &it : v) std::cout << it << std::endl;
}

// [[Rcpp::export]]
NumericVector ToySurveyArgDF(const DataFrame &df)
{
  std::mt19937 gen(std::random_device{}());
  std::normal_distribution<double> normal(0.0, 1.0);

  NumericVector numvec;
  for(int i = 0; i < 100; ++i) numvec.push_back(normal(gen));

  return numvec;
}
