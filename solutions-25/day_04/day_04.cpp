#include <iostream>

#include <libOc/grid.h>

using namespace std;

bool accessible(Grid<char> &g, Coord co) {
  size_t neighbour_rolls = 0;
  for (Coord n : co.ordinal_neighbours())
    neighbour_rolls += (g[n] == '@');
  return neighbour_rolls < 4;
}

size_t solve_p1(Grid<char> &g) {
  auto [n, m] = g.bound();
  size_t ret = 0;
  for (Coord c : g.coords()) {
    ret += g[c] == '@' && accessible(g, c);
  }
  return ret;
}

size_t solve_p2(Grid<char> &g) {
  auto [n, m] = g.bound();
  size_t ret = 0;

  std::vector<Coord> frontier = g.coords();

  for (Coord c; !frontier.empty(); c = frontier.back(), frontier.pop_back()) {
    char cur = g[c];
    if (cur != '@')
      continue;

    if (accessible(g, c)) {
      ret++;
      g[c] = '.';
      for (Coord n : c.ordinal_neighbours()) {
        frontier.push_back(n);
      }
    }
  }
  return ret;
}

int main() {
  Grid<char> g;
  cin >> g;
  g.pad('\0');
  cout << solve_p1(g) << '\n';
  cout << solve_p2(g) << '\n';
}
