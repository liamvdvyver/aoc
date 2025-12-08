#include <chrono>
#include <iostream>

#include <libOc/grid.h>

using namespace std;

constexpr bool accessible(Grid<bool> &g, Coord co) {
  size_t neighbour_rolls = 0;
  for (const Coord &o : ordinal_offsets) {
    const Coord n = co + o;
    if (g.in_bounds(n)) {
      neighbour_rolls += g.in_bounds(n) && g[n];
      if (neighbour_rolls >= 4)
        return false;
    }
  }
  return true;
}

size_t solve_p1(Grid<bool> &g) {
  auto [n, m] = g.bound();
  size_t ret = 0;
  for (Coord c : g.coords()) {
    ret += g[c] && accessible(g, c);
  }
  return ret;
}

size_t solve_p2(Grid<bool> &g) {
  auto [n, m] = g.bound();
  size_t ret = 0;

  std::vector<Coord> frontier = g.coords();

  for (Coord c = frontier.back(); !frontier.empty();
       c = frontier.back(), frontier.pop_back()) {

    // Just need check for first loop through
    if (g[c] && accessible(g, c)) {
      ret++;
      g[c] = false;

      for (const Coord &o : ordinal_offsets) {
        const Coord n = c + o;
        if (g.in_bounds(n) && g[n] && accessible(g, n)) {
          frontier.push_back(n);
        }
      }
    }
  }
  return ret;
}

int main() {
  auto start_time = chrono::steady_clock::now();
  Grid<bool> g = match_grid(cin, '@');
  cout << solve_p1(g) << '\n';
  cout << solve_p2(g) << '\n';
  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
