#include "iostream"
#include <cstdio>
#include <vector>

using namespace std;

typedef vector<vector<char>> puzzle_t;

void parse_stdin(puzzle_t &dest) {
  for (std::string line; std::getline(std::cin, line);) {

    vector<char> curline = vector<char>();
    for (char c : line) {
      curline.push_back(c);
    }
    dest.push_back(curline);
  }
}

int occ_here(const puzzle_t &puzzle, int puzzle_h, int puzzle_w, int x, int y,
             int dx, int dy, string str) {
  int len = str.length();

  int ret = 0;

  for (int i = 0; i < len; i++) {

    int x_c = x + dx * i;
    int y_c = y + dy * i;

    if (x_c < 0 || x_c >= puzzle_w || y_c < 0 || y_c >= puzzle_h) {
      return 0;
    }

    if (puzzle.at(y_c).at(x_c) != str.at(i)) {
      return 0;
    }
  }

  return 1;
}

int n_occ_from_loc(const puzzle_t &puzzle, int puzzle_h, int puzzle_w, int x,
                   int y, vector<int> dxs, vector<int> dys, string str) {

  int ret = 0;
  for (int dx : dxs) {
    for (int dy : dys) {
      if (dx == dy && dx == 0)
        continue;
      ret += occ_here(puzzle, puzzle_h, puzzle_w, x, y, dx, dy, str);
    }
  }

  return ret;
}

int solve_part_one(const puzzle_t &puzzle) {
  int ret = 0;

  int puzzle_h = puzzle.size();
  int puzzle_w = puzzle.front().size();

  vector<int> dxs = vector<int>{1, -1, 0};
  vector<int> dys = vector<int>{1, -1, 0};

  for (int y = 0; y < puzzle_h; y++) {
    for (int x = 0; x < puzzle_w; x++) {
      ret += n_occ_from_loc(puzzle, puzzle_h, puzzle_w, x, y, dxs, dys, "XMAS");
    }
  }
  return ret;
}

struct coord {
  int x;
  int y;
};

struct coord_pair {
  coord a;
  coord b;
};

struct am_as_offset_pair {
  coord_pair am;
  coord_pair as;
};

int xmas_at_loc(const puzzle_t &puzzle, int puzzle_h, int puzzle_w, int x,
                int y) {

  vector<am_as_offset_pair> pairs;

  coord_pair test = {{1, 1}};
  pairs.push_back({{{1, 1}, {1, -1}}, {{-1, 1}, {-1, -1}}});
  pairs.push_back({{{-1, 1}, {-1, -1}}, {{1, -1}, {1, 1}}});
  pairs.push_back({{{1, 1}, {-1, 1}}, {{1, -1}, {-1, -1}}});
  pairs.push_back({{{1, -1}, {-1, -1}}, {{1, 1}, {-1, 1}}});

  for (am_as_offset_pair p : pairs) {
    if (occ_here(puzzle, puzzle_h, puzzle_w, x, y, p.am.a.x, p.am.a.y, "AM") &&
        occ_here(puzzle, puzzle_h, puzzle_w, x, y, p.am.b.x, p.am.b.y, "AM") &&
        occ_here(puzzle, puzzle_h, puzzle_w, x, y, p.as.a.x, p.as.a.y, "AS") &&
        occ_here(puzzle, puzzle_h, puzzle_w, x, y, p.as.b.x, p.as.b.y, "AS")) {
      return 1;
    }
  }
  return 0;
}

int solve_part_two(const puzzle_t &puzzle) {
  int ret = 0;

  int puzzle_h = puzzle.size();
  int puzzle_w = puzzle.front().size();

  for (int y = 0; y < puzzle_h; y++) {
    for (int x = 0; x < puzzle_w; x++) {
      ret += xmas_at_loc(puzzle, puzzle_h, puzzle_w, x, y);
    }
  }
  return ret;
}

int main(int argc, char **argv) {

  puzzle_t puzzle = puzzle_t();
  parse_stdin(puzzle);
  cout << to_string(solve_part_one(puzzle)) << endl;
  cout << to_string(solve_part_two(puzzle)) << endl;
}
