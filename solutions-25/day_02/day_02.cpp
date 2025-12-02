#include <cmath>
#include <cstdint>
#include <iostream>
#include <string>

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

bool is_repetition(std::string seq) {
  for (int l = 1; l <= seq.length() / 2; l++) {
    if (seq.length() % l)
      continue;
    bool l_good = true;

    for (int i = 0; i < l; i++) {

      for (int a = 1; a < seq.length() / l; a++) {
        if (seq[i] != seq[a * l + i]) {
          l_good = false;
          break;
        }
      }

      if (!l_good)
        break;
    }

    if (l_good)
      return true;
  }
  return false;
}

uint64_t check_range_p2(string &start, string &end) {

  uint64_t l = stoull(start);
  uint64_t h = stoull(end);

  uint64_t ret = 0;

  // Length of numeric subsequence
  for (uint64_t len = 1; len <= end.length() / 2; len++) {

    // Each sequence of this length
    for (uint64_t seq = pow(10, len - 1); seq < pow(10, len); seq++) {
      if (is_repetition(to_string(seq)))
        continue;

      // Each possible repetition of this subsequence
      for (uint64_t cur = seq + seq * pow(10, len); cur <= h;
           cur = seq + (pow(10, len) * cur)) {
        if (cur >= l)
          ret += cur;
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
