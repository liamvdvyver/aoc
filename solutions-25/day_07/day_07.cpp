#include <cassert>
#include <iostream>
#include <string>
#include <vector>

#include <libOc/grid.h>

using namespace std;

int main(void) {
  vector<bool> beams;

  string ln;
  getline(cin, ln);

  for (char c : ln) {
    beams.push_back(c == 'S');
  }
  size_t n = beams.size();

  vector<size_t> n_ways(n, 0);

  for (int i = 0; i < n; i++) {
    n_ways[i] = beams[i];
  }

  vector<bool> splitters(n);
  size_t ret_1 = 0;

  while (!cin.eof()) {
    for (int i = 0; i < n; i++) {
      bool splitter = cin.get() == '^';

      if (n_ways[i] && splitter) {

        // Part one
        ret_1++;

        // Part two
        n_ways[i - 1] += n_ways[i];
        n_ways[i + 1] += n_ways[i];
        n_ways[i] = 0;
      }
    }
    cin.ignore();
  }

  size_t ret_2 = 0;
  for (auto n : n_ways) {
    ret_2 += n;
  }

  cout << ret_1 << '\n';
  cout << ret_2 << '\n';
}
