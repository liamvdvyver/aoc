#include <array>
#include <functional>
#include <iostream>
#include <queue>
#include <vector>

struct Coord : std::pair<int64_t, int64_t> {
  constexpr Coord(int64_t x, int64_t y) : std::pair<int64_t, int64_t>(x, y) {}
  constexpr Coord() : Coord(0, 0) {};
  constexpr Coord operator+(Coord b) {
    return Coord(this->first + b.first, this->second + b.second);
  }
  constexpr std::array<Coord, 4> cardinal_neighbours();
  constexpr std::array<Coord, 8> ordinal_neighbours();

  struct hash {
    size_t operator()(const Coord c) const {
      return (17 * (size_t)std::hash<int64_t>()(c.first)) ^
             std::hash<int64_t>()(c.second);
    }
  };
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
  bool in_bounds(Coord c) {
    return c.first >= 0 && c.second >= 0 && c.first < bound().first &&
           c.second < bound().second;
  }

  // Accessors

  // T &operator[](const Coord c) { return v[c.first][c.second]; }
  auto operator[](const Coord c) { return v[c.first][c.second]; }

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
  friend std::istream &operator>>(std::istream &is, Grid &g) {
    for (std::string ln; getline(is, ln);) {
      if (!ln.length())
        return is;

      g.v.emplace_back();
      g.v.back().reserve(ln.length());
      for (char c : ln) {
        g.v.back().push_back(c);
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

  std::queue<Coord> coords_q() {
    std::queue<Coord> ret;
    for (int i = 0; i < bound().first; i++) {
      for (int j = 0; j < bound().second; j++) {
        ret.push(Coord(i, j));
      }
    }
    return ret;
  }
};

static constexpr Grid<bool> match_grid(std::istream &is, const char match) {
  Grid<bool> g;
  for (std::string ln; getline(is, ln);) {
    if (!ln.length())
      return g;

    g.v.emplace_back();
    g.v.back().reserve(ln.length());
    for (char c : ln) {
      g.v.back().push_back(c == match);
    }
  }
  return g;
}
