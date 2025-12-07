#include <chrono>
#include <iostream>
#include <string>

using namespace std;

int main() {
  auto start_time = chrono::steady_clock::now();

  uint64_t ret_01 = 0;
  uint64_t ret_02 = 0;

  constexpr int64_t mod = 100;
  int64_t cur = 50;

  for (std::string ln; getline(cin, ln);) {

    int64_t sign = ln[0] == 'R' ? +1 : -1;
    int64_t n = stoi(ln.substr(1));
    int64_t prev = cur;

    cur += (n * sign);

    ret_02 += cur > 0 ? cur / mod : ((prev != 0) + (abs(cur) / mod));

    cur = ((cur % mod) + mod) % mod;

    if (!cur)
      ret_01++;
  }

  cout << ret_01 << '\n' << ret_02 << '\n';

  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
