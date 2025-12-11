#include <chrono>
#include <iostream>

#include <libOc/grid.h>
#include <libOc/parsing.h>
#include <queue>
#include <set>
#include <unordered_map>

using namespace std;

int main(void) {
  const auto start_time = chrono::steady_clock::now();

  vector<pair<int64_t, int64_t>> tiles = parse_pairs<int64_t>(cin);
  // vector<Coord> tiles;

  // tiles.reserve(tiles_.size());
  // for (auto [i, j] : tiles_) {
  //   tiles.emplace_back(i, j);
  // }
  const size_t n = tiles.size();

  // Part one

  size_t ret_1 = 0;

  for (int i = 0; i < n - 1; i++) {
    for (int j = i + 1; j < n; j++) {
      size_t candidate = (abs(tiles[i].first - tiles[j].first) + 1) *
                         (abs(tiles[i].second - tiles[j].second) + 1);
      ret_1 = max(ret_1, candidate);
    }
  }

  cout << ret_1 << '\n';

  // Part 2

  // Compress coordinates
  unordered_map<size_t, size_t> i_ranks;
  unordered_map<size_t, size_t> j_ranks;

  vector<size_t> i_vals_v;
  vector<size_t> j_vals_v;

  {
    // Implicit heap sort because I am feeling lazy
    set<size_t> i_vals;
    set<size_t> j_vals;
    for (auto [i, j] : tiles) {
      i_vals.insert(i);
      j_vals.insert(j);
    }

    // Insert into forward and reverse indices
    int idx = 0;
    for (auto i : i_vals) {
      if (!i_ranks.contains(i - 1)) {
        i_ranks[i - 1] = idx++;
        i_vals_v.push_back(i - 1);
      }
      if (!i_ranks.contains(i)) {
        i_ranks[i] = idx++;
        i_vals_v.push_back(i);
      }
      if (!i_ranks.contains(i + 1)) {
        i_ranks[i + 1] = idx++;
        i_vals_v.push_back(i + 1);
      }
    }

    idx = 0;
    for (auto j : j_vals) {
      if (!j_ranks.contains(j - 1)) {
        j_ranks[j - 1] = idx++;
        j_vals_v.push_back(j - 1);
      }
      if (!j_ranks.contains(j)) {
        j_ranks[j] = idx++;
        j_vals_v.push_back(j);
      }
      if (!j_ranks.contains(j + 1)) {
        j_ranks[j + 1] = idx++;
        j_vals_v.push_back(j + 1);
      }
    }
  }

  vector<Coord> comp_tiles;
  comp_tiles.reserve(n);
  for (auto [i, j] : tiles) {
    comp_tiles.emplace_back(i_ranks[i], j_ranks[j]);
  }

  // Build wall
  uint32_t imax = i_ranks.size();
  uint32_t jmax = j_ranks.size();
  Grid<bool> walls((vector<vector<bool>>(imax, vector<bool>(jmax, false))));
  Grid<bool> exterior((vector<vector<bool>>(imax, vector<bool>(jmax, false))));

  for (int idx = 0; idx < comp_tiles.size(); idx++) {
    Coord cur = comp_tiles[idx];
    auto [i, j] = cur;
    Coord next = comp_tiles[idx + 1 < comp_tiles.size() ? idx + 1 : 0];
    auto [i_next, j_next] = next;

    Coord offs;
    if (j == j_next && i < i_next) {
      offs = {1, 0};
    } else if (j == j_next && i > i_next) {
      offs = {-1, 0};
    } else if (i == i_next && j < j_next) {
      offs = {0, 1};
    } else if (i == i_next && j > j_next) {
      offs = {0, -1};
    } else {
      terminate();
    }
    // Build wall at cur
    walls[{i, j}] = true;

    // Build rest of wall
    while (i != i_next || j != j_next) {
      cur = cur + offs;
      i = cur.first;
      j = cur.second;
      walls[{i, j}] = true;
    }
  }

  // Flood fill exterior
  queue<Coord> frontier;
  for (int i = 0; i < imax; i++) {
    frontier.push({i, 0});
    frontier.push({i, jmax - 1});
  }
  for (int j = 0; j < jmax; j++) {
    frontier.push({0, j});
    frontier.push({imax - 1, j});
  }
  while (!frontier.empty()) {
    Coord cur = frontier.front();
    frontier.pop();

    if (!walls.in_bounds(cur) || walls[cur] || exterior[cur])
      continue;
    exterior[cur] = true;
    for (Coord n : cur.cardinal_neighbours()) {
      frontier.push(n);
    }
  }

  // Check shapes
  size_t ret_2 = 0;
  for (int idx = 0; idx < n; idx++) {
    for (int jdx = idx + 1; jdx < n; jdx++) {
      Coord a_comp = comp_tiles[idx];
      Coord b_comp = comp_tiles[jdx];

      // AAAAAHHHH
      int64_t cand = (abs((int64_t)i_vals_v[a_comp.first] -
                          (int64_t)i_vals_v[b_comp.first]) +
                      1) *
                     (abs((int64_t)j_vals_v[a_comp.second] -
                          (int64_t)j_vals_v[b_comp.second]) +
                      1);

      if (cand <= ret_2) {
        continue;
      }

      size_t ri_l = min(a_comp.first, b_comp.first);
      size_t ri_h = max(a_comp.first, b_comp.first);
      size_t rj_l = min(a_comp.second, b_comp.second);
      size_t rj_h = max(a_comp.second, b_comp.second);

      bool illegal = false;
      for (int ri = ri_l; ri <= ri_h; ri++) {
        for (int rj = rj_l; rj <= rj_h; rj++) {
          if (!walls[{ri, rj}] && exterior[{ri, rj}]) {
            illegal = true;
            break;
          }
        }
        if (illegal)
          break;
      }
      if (!illegal) {
        ret_2 = cand;
      }
    }
  }
  cout << ret_2 << '\n';

  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
