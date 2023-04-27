#include "surveygraph.h"

#include <iostream>
#include <fstream>
#include <string>

#include <sys/stat.h>

// Function: fileExists
/**
    Check if a file exists
@param[in] filename - the name of the file to check

@return    true if the file exists, else false

*/
bool fileExists(const std::string& filename)
{
  struct stat buf;

  if(stat(filename.c_str(), &buf) != -1) {
    return true;
  }
  return false;
}

void surveygraph::writesurvey()
{
  string filename = "results/survey.dat";

  ofstream myfile;
  myfile.open(filename);

  for(auto it : survey) {
    int u = it.first;

    map<int, int> r = it.second;

    myfile << u << " | ";

    for(int j = 0; j < n; ++j) {
      if(r.find(j) != r.end()) {
        myfile << r[j] << " ";
      }
      else {
        myfile << "NA ";
      }
    }
    myfile << '\n';
  }
  myfile.close();
}

void surveygraph::writerespondentgraph()
{
  string filename = "results/graph1.dat";

  ofstream myfile;
  myfile.open(filename);

  for(auto &it : G) {
    myfile << it.first << " : ";

    for(auto &jt : it.second) {
      myfile << jt.u << "[" << jt.w << "] ";
    }
    myfile << '\n';
  }
  myfile.close();
}

void surveygraph::writeitemgraph()
{
  string filename = "results/graph2.dat";

  ofstream myfile;
  myfile.open(filename);

  for(auto &it : H) {
    myfile << it.first << " : ";

    for(auto &jt : it.second) {
      myfile << jt.u << "[" << jt.w << "] ";
    }
    myfile << '\n';
  }
  myfile.close();
}
