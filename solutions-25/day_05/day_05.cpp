#include <iostream>
#include <vector>

#include "libOc/parsing.h"

using namespace std;

int main(void) {
  auto ranges = parse_delim<uint64_t>(cin);
  auto ingredients = lines<uint64_t>(cin);

  // Sort ranges/ingredients
  sort(ranges.begin(), ranges.end(),
       [](auto &x, auto &y) { return x[0] < y[0]; });
  sort(ingredients.begin(), ingredients.end());

  // Merge ranges
  size_t i = 0;
  for (size_t j = 1; j < ranges.size(); j++) {
    // Non-overlapping
    if (ranges[i][1] < ranges[j][0]) {
      ranges[++i] = ranges[j];
    }

    // Partial overlap
    else if (ranges[i][1] < ranges[j][1]) {
      ranges[i][1] = ranges[j][1];
    }
  }
  ranges.resize(i + 1);

  uint64_t ret_1 = 0;
  for (size_t i = 0, r = 0; r < ranges.size() && i < ingredients.size();) {
    if (ranges[r][0] <= ingredients[i] && ingredients[i] <= ranges[r][1]) {
      ret_1++;
      i++;
    } else if (ingredients[i] < ranges[r][0]) {
      i++;
    } else if (ingredients[i] >= ranges[r][1]) {
      r++;
    }
  }

  uint64_t ret_2 = 0;
  for (auto r : ranges) {
    ret_2 += r[1] - r[0] + 1;
  }
  cout << ret_1 << '\n';
  cout << ret_2 << '\n';
}
