#include "grid.h"

std::ostream &operator<<(std::ostream &os, const coord_t a) {
  return os << "(" << std::to_string(a.x) << ", " << std::to_string(a.y) << ")";
};

std::vector<coord_t> row_major(coord_t ub) {
  std::vector<coord_t> ret;
  for (idx_t y = 0; y < ub.y; y++) {
    for (idx_t x = 0; x < ub.x; x++) {
      ret.push_back({x, y});
    }
  }
  return ret;
}

Grid<char> gridify(std::istream &input) {

  // Allocate to nested vector
  std::vector<std::vector<char>> input_vs;

  for (std::string line; std::getline(input, line);) {
    std::vector<char> line_v;
    for (char c : line) {
      line_v.push_back(c);
    }
    input_vs.push_back(line_v);
  }

  // Convert to grid
  coord_t bounds = {(idx_t)input_vs.at(0).size(), (idx_t)input_vs.size()};

  Grid<char> ret = Grid<char>(bounds);
  for (coord_t c : row_major(bounds)) {
    ret.at(c) = input_vs.at(c.y).at(c.x);
  }
  return ret;
};

std::ostream &operator<<(std::ostream &os, const Grid<char> g) {
  const coord_t ub = g.bounds;
  for (idx_t y = 0; y < ub.y; y++) {
    std::string curLine = "";
    for (idx_t x = 0; x < ub.x; x++) {
      curLine += g.at({x, y});
    }
    os << curLine;
    if (y != g.bounds.y - 1) {
      os << std::endl;
    }
  }
  return os;
};
