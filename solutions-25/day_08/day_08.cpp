#include <chrono>
#include <iostream>
#include <vector>

#include <libOc/parsing.h>

using namespace std;

// Up to monotonic transofrm
constexpr size_t dist(const vector<size_t> &a, const vector<size_t> &b) {
  const size_t d0 = a[0] - b[0];
  const size_t d1 = a[1] - b[1];
  const size_t d2 = a[2] - b[2];
  return d0 * d0 + d1 * d1 + d2 * d2;
}

constexpr size_t find(size_t i, vector<size_t> &parents) {
  if (parents[i] == i)
    return i;

  size_t parent = find(parents[i], parents);
  parents[i] = parent;
  return parent;
}

constexpr pair<size_t, size_t> solve(vector<vector<size_t>> &coord) {

  pair<size_t, size_t> ret{};

  // Part one

  vector<pair<size_t, size_t>> pairs;
  vector<size_t> parents(coord.size());

  for (int i = 0; i < coord.size(); i++) {
    parents[i] = i;
    for (int j = i + 1; j < coord.size(); j++) {
      pairs.emplace_back(i, j);
    }
  }

  sort(pairs.begin(), pairs.end(),
       [&](const pair<size_t, size_t> &a, const pair<size_t, size_t> &b) {
         return dist(coord[a.first], coord[a.second]) >
                dist(coord[b.first], coord[b.second]);
       });

  // Example sizes are not part of input for this one
  const size_t n_pairs = coord.size() > 20 ? 1000 : 10;

  vector<size_t> group_sizes(coord.size(), 1);

  // Perform merges
  for (int pair_i = 1, merged = 0;; pair_i++) {

    bool part_one_done = false;

    if (pairs.empty())
      break;

    // Get pair
    auto [a, b] = pairs.back();
    pairs.pop_back();

    size_t rep_a = find(a, parents), rep_b = find(b, parents);
    if (rep_a == rep_b)
      continue;

    // Merge: reparent a (small) under b (big)
    if (!part_one_done) {
      if (group_sizes[rep_a] > group_sizes[rep_b]) {
        swap(rep_a, rep_b);
      }
      group_sizes[rep_b] += group_sizes[rep_a];
    }

    parents[rep_a] = rep_b;
    merged++;

    // Part one answer
    if (!part_one_done && pair_i == n_pairs) {

      sort(group_sizes.begin(), group_sizes.end(),
           [](size_t a, size_t b) { return a > b; });

      ret.first = group_sizes[0] * group_sizes[1] * group_sizes[2];

      part_one_done = true;
    }

    if (!part_one_done && merged == coord.size() - 1) {
      ret.second = coord[a][0] * coord[b][0];
      break;
    }
  }
  return ret;
};

int main(void) {
  const auto start_time = chrono::steady_clock::now();

  auto ds = parse_delim(cin);
  auto [ret_1, ret_2] = solve(ds);
  cout << ret_1 << '\n' << ret_2 << '\n';

  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
