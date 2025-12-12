#include <chrono>
#include <iostream>
#include <libOc/grid.h>
#include <sstream>

using namespace std;

int main(void) {
  const auto start_time = chrono::steady_clock::now();

  vector<Grid<bool>> shapes;
  vector<pair<Coord, vector<size_t>>> regions;
  int n_shapes = 6;

  for (int i = 0; i < n_shapes; i++) {
    std::string ln;
    getline(cin, ln);
    shapes.push_back(match_grid(cin, '#'));
    // cout << shapes.back();
  }

  vector<size_t> shape_sz;
  for (auto &s : shapes) {
    shape_sz.push_back(0);
    for (auto bv : s.v) {
      for (auto b : bv) {
        shape_sz.back() += b;
      }
    }
  }

  for (string ln; getline(cin, ln) && ln.length();) {
    // cout << ln;
    stringstream ss{ln};
    char ignore;
    regions.emplace_back();
    ss >> regions.back().first.first;
    ss.ignore();
    ss >> regions.back().first.second;
    ss.ignore();
    for (int i = 0; i < n_shapes; i++) {
      regions.back().second.emplace_back();
      ss >> regions.back().second.back();
    }
  }

  size_t feas = 0;
  for (auto &r : regions) {
    size_t cost = 0;
    size_t cap = r.first.first * r.first.second;
    for (int i = 0; i < n_shapes; i++) {
      cost += shape_sz[i] * r.second[i];
    }
    if (cost <= cap)
      feas++;
  }
  cout << feas << '\n';

  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
