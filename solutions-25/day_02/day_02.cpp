#include <chrono>
#include <cstdint>
#include <iostream>
#include <unordered_set>

using namespace std;

template <typename T> constexpr T dec_len(T x) {
  T ret = 0;
  for (; x; ret++, x /= 10) {
  };
  return ret;
}

template <typename T> constexpr T ten_msk(T x) {
  T ret = 1;
  for (; x; ret *= 10, x /= 10) {
  };
  return ret;
}

template <typename T> constexpr T half_ten_msk(T x) {
  T ret = 1;
  for (; x; ret *= 10, x /= 100) {
  };
  return ret;
}

constexpr uint64_t check_range_p1(uint64_t l, const uint64_t h) {
  uint64_t ret = 0;

  uint64_t cur_half = l / half_ten_msk(l);
  uint64_t cur_half_ten_msk = ten_msk(cur_half);
  uint64_t cur = cur_half * cur_half_ten_msk + cur_half;

  while ((cur = cur_half * cur_half_ten_msk + cur_half) <= h) {
    if (cur >= l)
      ret += cur;
    cur_half++;

    if (cur_half / cur_half_ten_msk)
      cur_half_ten_msk = ten_msk(cur_half);
  }
  return ret;
}

constexpr uint64_t check_range_p2(const uint64_t l, const uint64_t h) {

  std::unordered_set<uint64_t> seen;

  uint64_t ret = 0;
  const uint64_t end_len = dec_len(h) / 2;

  // Length of numeric subsequence
  for (uint64_t len = 1, pow_l = 1, pow_h = 10; len <= end_len;
       len++, pow_l *= 10, pow_h *= 10) {

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
  auto start_time = chrono::steady_clock::now();

  uint64_t ret_1 = 0;
  uint64_t ret_2 = 0;

  char ignore;
  uint64_t l, h;
  while (cin >> l >> ignore >> h) {

    ret_1 += check_range_p1(l, h);
    ret_2 += check_range_p2(l, h);

    cin.ignore();
  }

  cout << ret_1 << '\n' << ret_2 << '\n';

  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
