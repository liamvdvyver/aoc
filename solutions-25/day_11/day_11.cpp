#include <iostream>
#include <sstream>
#include <unordered_map>
#include <vector>

using namespace std;
using adj_t = std::unordered_map<uint64_t, vector<uint64_t>>;

const uint64_t you = ('y' << 16 | 'o' << 8 | 'u');
const uint64_t out = ('o' << 16 | 'u' << 8 | 't');
const uint64_t svr = ('s' << 16 | 'v' << 8 | 'r');
const uint64_t dac = ('d' << 16 | 'a' << 8 | 'c');
const uint64_t fft = ('f' << 16 | 'f' << 8 | 't');

// Part 1

size_t dfs_visit(adj_t &adj, std::unordered_map<uint64_t, size_t> &dp,
                 uint64_t cur, uint64_t goal, uint64_t avoid = 0) {
  if (cur == avoid)
    return 0;
  if (dp.count(cur))
    return dp[cur];
  if (cur == goal)
    return 1;
  size_t ret = 0;
  for (uint64_t next : adj[cur]) {
    ret += dfs_visit(adj, dp, next, goal);
  }
  dp[cur] = ret;
  return ret;
};

size_t solve_p1(adj_t &adj) {
  std::unordered_map<uint64_t, size_t> dp;
  return dfs_visit(adj, dp, you, out);
};

// Part 2

size_t solve_p2(adj_t &adj) {
  unordered_map<uint64_t, unordered_map<uint64_t, size_t>> dp;
  unordered_map<uint64_t, size_t> dac_dp;
  unordered_map<uint64_t, size_t> fft_dp;
  unordered_map<uint64_t, size_t> out_dp;

  size_t svr_to_dac = dfs_visit(adj, dac_dp, svr, dac);
  size_t fft_to_dac = dfs_visit(adj, dac_dp, fft, dac);

  size_t svr_to_fft = dfs_visit(adj, fft_dp, svr, fft);
  size_t dac_to_fft = dfs_visit(adj, fft_dp, dac, fft);

  size_t dac_to_out = dfs_visit(adj, out_dp, dac, out);
  size_t fft_to_out = dfs_visit(adj, out_dp, fft, out);

  return svr_to_dac * dac_to_fft * fft_to_out +
         svr_to_fft * fft_to_dac * dac_to_out;
};

int main(void) {
  adj_t adj;

  for (string ln; getline(cin, ln);) {
    stringstream ss(ln);
    char a, b, c;
    ss >> a >> b >> c;
    uint64_t cur = (a << 16 | b << 8 | c);
    ss.ignore(2);
    adj[cur];

    while (ss >> a >> b >> c) {
      uint64_t to = (a << 16 | b << 8 | c);
      adj[cur].push_back(to);
      ss.ignore();
    }
  }

  cout << solve_p1(adj) << endl;
  cout << solve_p2(adj) << endl;
}
