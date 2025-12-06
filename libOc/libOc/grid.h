#include <array>
#include <iostream>
#include <vector>

struct Coord : std::pair<int64_t, int64_t> {
  Coord(int64_t x, int64_t y) : std::pair<int64_t, int64_t>(x, y) {}
  Coord() : Coord(0, 0) {};
  constexpr Coord operator+(Coord b) {
    return Coord(this->first + b.first, this->second + b.second);
  }
  constexpr std::array<Coord, 4> cardinal_neighbours();
  constexpr std::array<Coord, 8> ordinal_neighbours();
};

static const std::array<Coord, 4> cardinal_offsets = {
    Coord(1, 0), Coord(-1, 0), Coord(0, 1), Coord(0, -1)};
static const std::array<Coord, 4> intercardinal_offsets = {
    Coord(1, 1), Coord(-1, 1), Coord(1, -1), Coord(-1, -1)};
static const std::array<Coord, 8> ordinal_offsets = {
    Coord(1, 0), Coord(-1, 0), Coord(0, 1),  Coord(0, -1),
    Coord(1, 1), Coord(-1, 1), Coord(1, -1), Coord(-1, -1)};

constexpr std::array<Coord, 4> Coord::cardinal_neighbours() {
  std::array<Coord, 4> ret;
  for (int i = 0; i < 4; i++) {
    ret[i] = *this + cardinal_offsets[i];
  }
  return ret;
};
constexpr std::array<Coord, 8> Coord::ordinal_neighbours() {
  std::array<Coord, 8> ret;
  for (int i = 0; i < 8; i++) {
    ret[i] = *this + ordinal_offsets[i];
  }
  return ret;
};

template <typename T> struct Grid {

  constexpr Grid(std::vector<std::vector<T>> v) : v(v) {}

  constexpr Grid() {}

  std::vector<std::vector<T>> v;

  void pad(T val) {
    const size_t n_x = bound().first;

    for (auto &u : v) {
      u.insert(u.begin(), val);
      u.push_back(val);
    }

    v.emplace(v.begin(), n_x + 2, val);
    v.emplace_back(n_x + 2, val);
  }

  Coord bound() {
    if (v.size() == 0)
      return {};
    return Coord(v.size(), v[0].size());
  }

  // Accessors

  T &operator[](const Coord c) { return v[c.first][c.second]; }

  const T &operator[](const Coord c) const { return v.at(c.first)(c.second); }

  // Printing
  friend std::ostream &operator<<(std::ostream &os, const Grid &g) {
    for (auto &u : g.v) {
      for (auto e : u) {
        os << e;
      }
      os << '\n';
    }
    return os;
  }

  // Read until double newline or eof
  friend std::istream &operator>>(std::istream &is, Grid<char> &g) {
    g.v.emplace_back();
    while (true) {
      if (is.peek() == '\n' && g.v.empty())
        break;
      if (is.peek() == '\n' && g.v.back().empty() || is.eof()) {
        g.v.pop_back();
        break;
      }

      if (is.peek() == '\n') {
        is.ignore();
        g.v.emplace_back();
      } else {
        g.v.back().emplace_back();
        g.v.back().back() = is.get();
        // is >> g.v.back().back();
      }
    }
    return is;
  }

  std::vector<Coord> coords() {
    std::vector<Coord> ret;
    ret.reserve(bound().first * bound().second);
    for (int i = 0; i < bound().first; i++) {
      for (int j = 0; j < bound().second; j++) {
        ret.push_back(Coord(i, j));
      }
    }
    return ret;
  }
};
