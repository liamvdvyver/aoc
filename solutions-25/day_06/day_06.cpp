#include <cassert>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <libOc/grid.h>
#include <libOc/parsing.h>

using namespace std;

uint64_t solve_p1(vector<vector<uint64_t>> &nums, vector<char> &ops) {
  vector<uint64_t> acc = nums[0];
  for (int i = 1; i < nums.size(); i++) {
    for (int j = 0; j < ops.size(); j++) {
      if (ops[j] == '*') {
        acc[j] *= nums[i][j];
      } else if (ops[j] == '+') {
        acc[j] += nums[i][j];
      }
    }
  }
  uint64_t ret = 0;
  for (auto n : acc) {
    ret += n;
  }
  return ret;
}

uint64_t solve_p2(Grid<char> &g) {
  uint64_t ret = 0;
  int64_t cur = -1;

  // Over each column
  for (int j = 0; j < g.bound().second; j++) {

    uint64_t col_num = 0;
    char op = g[{g.bound().first - 1, j}];

    // Over each row, build up number
    for (int i = 0; i < g.bound().first - 1; i++) {
      // cout << g[{i, j}] << endl;
      if (g[{i, j}] != ' ') {
        col_num *= 10;
        col_num += g[{i, j}] - '0';
      }
    }

    if (!col_num) {
      assert(cur >= 0);
      ret += cur;
      cur = -1;
    } else if (cur == -1) {
      cur = col_num;
    } else if (op == '+') {
      cur += col_num;
    } else if (op == '*') {
      cur *= col_num;
    }
  }
  return ret;
}

int main(void) {

  // Grid/nums for parts 2/1
  vector<vector<uint64_t>> nums;
  Grid<char> g;
  vector<char> ops;

  for (std::string l; getline(cin, l);) {

    // Grid
    g.v.emplace_back();
    for (auto c : l) {
      g.v.back().emplace_back(c);
    }

    // Parse ops
    if (l.find('*') != l.npos) {
      for (auto c : l) {
        if (c == '*' || c == '+') {
          ops.push_back(c);
        }
      }

      // Parse nums
    } else {
      nums.emplace_back();
      stringstream ss{l};
      uint64_t n;
      while (ss >> n) {
        nums.back().push_back(n);
      }
    }
  }

  cout << solve_p1(nums, ops) << '\n';

  // Add empty column
  for (auto &u : g.v) {
    u.push_back(' ');
  }

  // Fill in operators
  char cur = ' ';
  for (char &c : g.v.back()) {
    if (c != ' ')
      cur = c;
    c = cur;
  }

  cout << solve_p2(g) << '\n';
}
