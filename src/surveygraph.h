#ifndef SURVEYGRAPH_H_
#define SURVEYGRAPH_H_

#include <vector>
#include <map>
#include <set>

using namespace std;

struct neighbour
{
  neighbour(int a, double b) { u = a; w = b; }

  int u;     // neighbour index
  int olap;  // overlap weight
  double w;  // euclidean distance or cosine similarity weight
  double c;  // covariance

  // order by neighbour index, u
  bool operator<(const neighbour& rhs) const { return u < rhs.u; }
};

class surveygraph
{
  public :
    surveygraph () {}

    int m, n;   // number of respondents, items

    vector<vector<double>> surveyvec;           // survey in vector format
    map<int, map<int, int>> survey;             // survey in map format
    map<int, set<neighbour>> g_respondents;     // respondent graph
    map<int, set<neighbour>> g_items;           // item graph
    
    map<int, map<int, double>> likert_1_5;

    void pilot();

    void inputdf(const int &);

    // synthetic survey method
    void build_survey_synthetic();

    // graph construction methods
    void build_pilot();
    void build_g_items();
    void build_g_respondents();

    void sparse_g_items();
    void sparse_g_respondents();

    void item_overlap(const int&, const int&, double&);
    void item_euclid(const int&, const int&, double&);

    void respondent_overlap(const int&, const int&, double&);
    void respondent_euclid(const int&, const int&, double&);
};
#endif
