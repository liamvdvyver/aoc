#include <cmath>
#include <iostream>
#include <string>

using namespace std;

uint64_t max_joltage(const std::string_view bank, const size_t n) {
  if (!n)
    return 0;

  std::string_view sub_bank = bank.substr(0, bank.size() + 1 - n);
  size_t max_pos = 0;
  char max = 0;

  for (char c = '9'; c >= '0'; c--) {
    max_pos = sub_bank.find(c);
    if (max_pos != sub_bank.npos) {
      auto ret = pow(10, n - 1) * (c - '0') +
                 max_joltage(bank.substr(max_pos + 1), n - 1);
      return ret;
    }
  }
  return -1;
}

int main() {
  std::string ln;

  uint64_t ret_1 = 0;
  uint64_t ret_2 = 0;

  while (std::cin >> ln) {
    ret_1 += max_joltage(ln, 2);
    ret_2 += max_joltage(ln, 12);
  }

  std::cout << ret_1 << '\n';
  std::cout << ret_2 << '\n';
}
