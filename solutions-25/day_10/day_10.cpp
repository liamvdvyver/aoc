#include <iostream>
#include <vector>

#include <z3++.h>

using namespace std;

struct Machine {
  uint64_t goal;
  vector<uint64_t> actions_bsets;
  vector<vector<uint64_t>> action_joltages;
  vector<uint64_t> joltages;
};

// Part one

bool search_p1(Machine &m, uint64_t cur, size_t depth) {
  // cout << cur << endl;
  if (depth == 0)
    return cur == m.goal;
  for (size_t action : m.actions_bsets) {
    if (search_p1(m, cur ^ action, depth - 1))
      return true;
  }
  return false;
}

size_t solve_p1(Machine &m) {
  // Brute force that bitch
  for (int d = 0; d < m.actions_bsets.size(); d++) {
    uint64_t cur = 0;
    if (search_p1(m, cur, d))
      return d;
  }
  return false;
}

size_t solve_p1(vector<Machine> &ms) {
  size_t ret = 0;
  for (Machine &m : ms) {
    ret += solve_p1(m);
  }
  return ret;
}

// Part two

size_t solve_p2(Machine &m) {

  z3::context c;
  z3::solver s(c);

  z3::expr zero = c.int_const("zero");
  s.add(zero == 0);

  // Add vars: num times action selected
  vector<z3::expr> actions;
  vector<z3::expr> partial_costs;
  for (int i = 0; i < m.action_joltages.size(); i++) {
    actions.push_back(c.int_const(format("n_action_{}", i).c_str()));

    z3::expr prev_sum = i ? partial_costs.back() : zero;

    partial_costs.push_back(c.int_const(format("n_action_{}_sum", i).c_str()));
    s.add(actions.back() >= 0);

    s.add(prev_sum + actions.back() == partial_costs.back());
  }

  vector<vector<z3::expr>> sums;
  for (int action_idx = 0; action_idx < m.action_joltages.size();
       action_idx++) {

    sums.emplace_back();
    vector<z3::expr> &cur_joltages = sums.back();

    for (int joltage_idx = 0; joltage_idx < m.joltages.size(); joltage_idx++) {

      z3::expr prev_joltage =
          action_idx ? sums[action_idx - 1][joltage_idx] : zero;
      sums.back().push_back(c.int_const(
          format("joltage_{}_action_{}", joltage_idx, action_idx).c_str()));

      // TODO: optimise if neccesary
      auto cur_action_joltages = m.action_joltages[action_idx];
      bool cur_included =
          find(cur_action_joltages.begin(), cur_action_joltages.end(),
               joltage_idx) != cur_action_joltages.end();

      s.add(prev_joltage + (int)cur_included * actions[action_idx] ==
            sums.back().back());

      // Contstrain final joltages
      if (action_idx == m.action_joltages.size() - 1) {
        s.add(sums.back().back() == (int)m.joltages[joltage_idx]);
      }
    }
  }

  // Add constraints: final sum
  for (int d = 0; d < 999; d++) {
    s.push();
    z3::expr d_lim = partial_costs.back() == d;
    s.add(d_lim);
    if (s.check() == z3::sat) {
      return d;
    }
    s.pop();
  }
  return 0;
}

size_t solve_p2(vector<Machine> &ms) {
  size_t ret = 0;
  int i = 0;
  for (Machine &m : ms) {
    ret += solve_p2(m);
  }
  return ret;
}

int main(void) {
  vector<Machine> input;
  while (!cin.eof()) {

    cin.ignore();

    if (cin.eof())
      break;

    input.emplace_back();

    uint64_t cur = 1;
    for (char c; (c = cin.get()) != ']'; cur <<= 1) {
      if (c == '#') {
        input.back().goal ^= cur;
      }
    }

    cin.ignore();              // ' '
    while (cin.get() != '{') { // '('
      uint64_t cur_action = 0;
      input.back().action_joltages.emplace_back();
      while (true) {
        uint64_t shift;
        cin >> shift;
        cur_action ^= (1 << shift);
        input.back().action_joltages.back().push_back(shift);
        if (cin.get() == ')') { // ','
          cin.ignore();
          break;
        }
      }
      input.back().actions_bsets.push_back(cur_action);
    }

    while (true) {
      uint64_t cur_j = 0;
      cin >> cur_j;
      input.back().joltages.push_back(cur_j);
      if (cin.get() == '}') {
        break;
      }
    }

    cin.ignore();

    Machine &m = input.back();
    for (auto &a : m.action_joltages) {
      for (auto j : a) {
      }
    }

    for (uint64_t j : m.joltages) {
    }
  }

  cout << solve_p1(input) << endl;
  cout << solve_p2(input) << endl;
}
