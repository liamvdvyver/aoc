#include <iostream>
#include <vector>

using Coord = std::pair<size_t, size_t>;
constexpr Coord operator+(Coord a, Coord b) {
  return std::make_pair(a.first + b.first, a.second + b.second);
}

using C = Coord;

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
      return std::make_pair(0, 0);
    return std::make_pair(v[0].size(), v.size());
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
        is >> g.v.back().back();
      }
    }
    return is;
  }
};
