#ifndef SURVEYGRAPH_H_
#define SURVEYGRAPH_H_

#include <vector>
#include <map>
#include <set>

using namespace std;

struct neighbour
{
  neighbour(int a, int b) { u = a; w = b; }

  int u;     // neighbour index
  int w;     // overlap weight
  double c;  // covariance

  // order by neighbour index, u
  bool operator<(const neighbour& rhs) const { return u < rhs.u; }
};

class surveygraph
{
  public :
    surveygraph () {}

    int m, n;   // number of respondents, items

    vector<vector<double>> surveyvec;    // survey in vector format
    map<int, map<int, int>> survey;   // survey in map format
    map<int, set<neighbour>> G, H;    // respondent and item graphs

    void pilot();
    void dummy(const int&, const int&);

    void inputdf(const int &);

    // building methods
    void buildsynthetic();
    void builditemgraph();
    void buildrespondentgraph();

    void item_overlap(const int&, const int&, int&);
    void item_euclid(const int&, const int&, double&);

    void respondent_overlap(const int&, const int&, int&);
    void respondent_euclid(const int&, const int&, double&);

    // printing methods
    void printitemgraph();
    void printrespondentgraph();
    void printsurvey();

    // writing methods
    void writeitemgraph();
    void writerespondentgraph();
    void writesurvey();
};
#endif
