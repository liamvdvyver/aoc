#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

void parse_reports(std::vector<std::vector<int>> &dest) {
  for (std::string line; std::getline(std::cin, line);) {
    std::vector<int> cur{};

    int i = line.find(" ");
    while (i != std::string::npos) {
      i = line.find(" ");
      std::string cur_num = i == std::string::npos ? line : line.substr(0, i);
      cur.push_back(std::stoi(cur_num));
      line.erase(0, i + 1);
    }

    dest.push_back(cur);
  }
}

bool is_safe(const std::vector<int> &report_line) {

  int prev = 0;
  int diff = 0;

  int last_diff = 0;

  int n_itr = 0;

  for (auto &num : report_line) {

    last_diff = diff;
    diff = num - prev;

    if (n_itr > 0) {
      if (diff == 0)
        return false;

      if (abs(diff) > 3)
        return false;
    }

    if (n_itr > 1 && diff * last_diff < 0) {
      return false;
    }

    prev = num;

    n_itr++;
  }
  return true;
}

int solve_part_one(std::vector<std::vector<int>> &reports) {
  int ret = 0;
  for (auto &line : reports) {

    if (is_safe(line)) {
      ret++;
    }
  }

  return ret;
}

bool is_safe_dampened(const std::vector<int> &report_line) {
    if (is_safe(report_line)) return true;

    for (int i = 0; i < report_line.size(); i++) {
        std::vector<int> line_cpy = report_line;
        line_cpy.erase(line_cpy.begin() + i);
        if (is_safe(line_cpy)) return true;
    }

    return false;

}

int solve_part_two(std::vector<std::vector<int>> &reports) {
  int ret = 0;
  for (auto &line : reports) {

    if (is_safe_dampened(line)) {
      ret++;
    }
  }

  return ret;

}

int main(int argc, char **argv) {
  std::vector<std::vector<int>> lines = std::vector<std::vector<int>>();
  parse_reports(lines);

  std::cout << std::to_string(solve_part_one(lines)) << std::endl;
  std::cout << std::to_string(solve_part_two(lines)) << std::endl;
}
