#include <chrono>
#include <iostream>
#include <vector>

using namespace std;

int main(void) {
  const auto start_time = chrono::steady_clock::now();

  string ln;
  getline(cin, ln);
  const size_t n = ln.length();

  vector<uint64_t> ways(n, 0);
  ways[ln.find('S')] = 1;

  size_t ret_1 = 0;
  while (getline(cin, ln)) {
    for (int i = 0; i < n; i++) {
      if (ways[i] && ln[i] == '^') {
        ret_1++;
        ways[i - 1] += ways[i];
        ways[i + 1] += ways[i];
        ways[i] = 0;
      }
    }
  }
  size_t ret_2 = 0;
  for (auto n : ways)
    ret_2 += n;

  cout << ret_1 << '\n' << ret_2 << '\n';

  auto end_time = chrono::steady_clock::now();
  cerr << chrono::duration<double, milli>(end_time - start_time) << '\n';
}
