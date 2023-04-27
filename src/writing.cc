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

void surveygraph::write_survey()
{
  string filename = "results/survey.dat";

  ofstream myfile;
  myfile.open(filename);

  for(auto it : survey) {
    int u = it.first;

    map<int, int> r = it.second;

    myfile << u << " | ";
    for(int j = 0; j < n; ++j) {
      if(r.find(j) != r.end())
        myfile << r[j] << " ";
      else
        myfile << "NA ";
    }
    myfile << '\n';
  }
  myfile.close();
}

void surveygraph::write_g_respondents()
{
  string filename = "/Users/sam/surveygraphr/results/graph1.dat";

  ofstream myfile;
  myfile.open(filename);

  for(auto &it : g_respondents) {
    myfile << it.first << " : ";
    for(auto &jt : it.second)
      myfile << jt.u << "[" << jt.w << "] ";
    myfile << '\n';
  }
  myfile.close();
}

void surveygraph::write_g_items()
{
  string filename = "results/graph2.dat";

  ofstream myfile;
  myfile.open(filename);

  for(auto &it : g_items) {
    myfile << it.first << " : ";
    for(auto &jt : it.second)
      myfile << jt.u << "[" << jt.w << "] ";
    myfile << '\n';
  }
  myfile.close();
}
