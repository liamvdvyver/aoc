#include <chrono>
#include <iostream>
#include <vector>

#include "libOc/parsing.h"

using namespace std;

int main(void) {
  auto start_time = chrono::steady_clock::now();
  auto ranges = parse_pairs<uint64_t>(cin);
  auto ingredients = lines<uint64_t>(cin);

  // Sort ranges/ingredients
  sort(ranges.begin(), ranges.end(),
       [](auto &x, auto &y) { return x.first < y.first; });
  sort(ingredients.begin(), ingredients.end());

  // Merge ranges
  size_t i = 0;
  for (size_t j = 1; j < ranges.size(); j++) {
    // Non-overlapping
    if (ranges[i].second < ranges[j].first) {
      ranges[++i] = ranges[j];
    }

    // Partial overlap
    else if (ranges[i].second < ranges[j].second) {
      ranges[i].second = ranges[j].second;
    }
  }
  ranges.resize(i + 1);

  uint64_t ret_1 = 0;
  for (size_t i = 0, r = 0; r < ranges.size() && i < ingredients.size();) {
    if (ranges[r].first <= ingredients[i] &&
        ingredients[i] <= ranges[r].second) {
      ret_1++;
      i++;
    } else if (ingredients[i] < ranges[r].first) {
      i++;
    } else if (ingredients[i] >= ranges[r].second) {
      r++;
    }
  }

  uint64_t ret_2 = 0;
  for (auto r : ranges) {
    ret_2 += r.second - r.first + 1;
  }
  cout << ret_1 << '\n';
  cout << ret_2 << '\n';
  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
