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

    int m, n;   // number of respondents, questions

    vector<vector<int>> surveyvec;    // survey in vector format
    map<int, map<int, int>> survey;   // survey in map format
    map<int, set<neighbour>> G, H;    // respondent and question graphs

    void pilot();
    void dummy(const int&, const int&);

    // building methods
    void buildsynthetic();
    void buildquestiongraph();
    void buildrespondentgraph();

    void questionoverlap(const int&, const int&, int&);
    void questioncov(const int&, const int&, double&);

    void respondentoverlap(const int&, const int&, int&);
    void respondentcov(const int&, const int&, double&);

    // printing methods
    void printquestiongraph();
    void printrespondentgraph();
    void printsurvey();

    // writing methods
    void writequestiongraph();
    void writerespondentgraph();
    void writesurvey();
};
#endif
