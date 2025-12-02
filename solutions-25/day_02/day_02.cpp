#include <cmath>
#include <cstdint>
#include <iostream>
#include <string>
#include <unordered_set>

using namespace std;

uint64_t check_range_p1(string &start, string &end) {
  uint64_t l = stoull(start);
  uint64_t h = stoull(end);

  uint64_t ret = 0;

  uint64_t cur_half =
      stoull(start.substr(0, max((size_t)1, start.length() / 2)));
  uint64_t cur = stoull(to_string(cur_half) + to_string(cur_half));

  while (cur <= h) {
    if (cur >= l)
      ret += cur;
    cur_half++;
    cur = stoull(to_string(cur_half) + to_string(cur_half));
  }
  return ret;
}

uint64_t check_range_p2(string &start, string &end) {

  std::unordered_set<uint64_t> seen;

  uint64_t l = stoull(start);
  uint64_t h = stoull(end);

  uint64_t ret = 0;

  // Length of numeric subsequence
  for (uint64_t len = 1; len <= end.length() / 2; len++) {

    uint64_t pow_l = pow(10, len - 1);
    uint64_t pow_h = 10 * pow_l;

    // Each sequence of this length
    for (uint64_t seq = pow_l; seq < pow_h; seq++) {

      // Each possible repetition of this subsequence
      for (uint64_t cur = seq + seq * pow_h; cur <= h;
           cur = seq + (pow_h * cur)) {

        if (cur >= l && !seen.count(cur)) {
          seen.insert(cur);
          ret += cur;
        }
      }
    }
  }
  return ret;
}

int main(void) {
  string start;
  string end;

  uint64_t ret_1 = 0;
  uint64_t ret_2 = 0;

  while (getline(cin, start, '-') && getline(cin, end, ',')) {

    // Remove newline
    if (end[end.length() - 1] == '\n') {
      end = end.substr(0, end.length() - 1);
    }

    ret_1 += check_range_p1(start, end);
    ret_2 += check_range_p2(start, end);
  }

  cout << ret_1 << '\n' << ret_2 << '\n';
}
