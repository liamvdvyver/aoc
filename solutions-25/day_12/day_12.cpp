#include <chrono>
#include <cstdio>
#include <ios>
#include <iostream>
#include <libOc/grid.h>
#include <limits>

using namespace std;

int main(void) {
  const auto start_time = chrono::steady_clock::now();

  vector<pair<Coord, vector<size_t>>> regions;

  const size_t n_shapes = 6;
  const size_t shape_sz = 3;

  vector<size_t> shapes(n_shapes);
  for (int i = 0; i < n_shapes; i++) {
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    while (cin.peek() != '\n') {
      for (char c; (c = cin.get()) != '\n';) {
        shapes[i] += c == '#';
      }
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
  }

  while (cin.peek() != EOF) {

    regions.emplace_back();
    cin >> regions.back().first.first;
    cin.ignore(); // x
    cin >> regions.back().first.second;
    cin.ignore(); // :

    for (auto i = 0; i < n_shapes; i++) {
      regions.back().second.emplace_back();
      cin >> regions.back().second.back();
    }

    cin.ignore();
  }

  size_t feas = 0;
  for (auto &r : regions) {
    size_t cost = 0;
    size_t cap = r.first.first * r.first.second;
    for (int i = 0; i < n_shapes; i++) {
      cost += shapes[i] * r.second[i];
    }
    if (cost <= cap)
      feas++;
  }
  cout << feas << '\n';

  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
