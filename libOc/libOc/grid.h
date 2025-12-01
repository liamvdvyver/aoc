#pragma once

#include "iostream"
#include <cassert>
#include <cstdint>
#include <vector>

typedef int idx_t;

// COORDINATES

typedef struct {
  idx_t x;
  idx_t y;
} coord_t;

constexpr coord_t operator+(const coord_t a, const coord_t b) {
  return {a.x + b.x, a.y + b.y};
}

constexpr coord_t operator-(const coord_t a, const coord_t b) {
  return {a.x - b.x, a.y - b.y};
}

constexpr bool operator<(const coord_t a, const coord_t b) {
  return a.x < b.x && a.y < b.y;
}

constexpr bool operator==(const coord_t a, const coord_t b) {
  return a.x == b.x && a.y == b.y;
}

constexpr bool operator!=(const coord_t a, const coord_t b) {
  return !(a == b);
}

constexpr bool operator>(const coord_t a, const coord_t b) { return b < a; }

constexpr bool operator<=(const coord_t a, const coord_t b) {
  return a.x <= b.x && a.y <= b.y;
}

constexpr bool operator>=(const coord_t a, const coord_t b) {
  return a.x >= b.x && a.y >= b.y;
}

constexpr coord_t operator*(const coord_t a, const idx_t c) {
  return {a.x * c, a.y * c};
};

constexpr coord_t operator*(const idx_t c, const coord_t a) { return a * c; };

constexpr coord_t operator/(const coord_t a, const idx_t c) {
  return {a.x / c, a.y / c};
};

std::ostream &operator<<(std::ostream &os, const coord_t a);

// DIRECTIONS

enum class Direction : uint8_t {

  UP = 0b1,
  DOWN = 0b10,
  LEFT = 0b100,
  RIGHT = 0b1000,

  UP_LEFT = 0b10000,
  UP_RIGHT = 0b100000,
  DOWN_LEFT = 0b1000000,
  DOWN_RIGHT = 0b10000000,

};

constexpr coord_t offset(Direction d) {
  switch (d) {
  case Direction::UP:
    return {0, -1};
  case Direction::DOWN:
    return {0, 1};
  case Direction::LEFT:
    return {-1, 0};
  case Direction::RIGHT:
    return {1, 0};
  case Direction::UP_LEFT:
    return offset(Direction::UP) + offset(Direction::LEFT);
  case Direction::UP_RIGHT:
    return offset(Direction::UP) + offset(Direction::RIGHT);
  case Direction::DOWN_LEFT:
    return offset(Direction::DOWN) + offset(Direction::LEFT);
  case Direction::DOWN_RIGHT:
    return offset(Direction::DOWN) + offset(Direction::RIGHT);
    break;
  }
}

std::vector<coord_t> row_major(coord_t ub);

template <typename T> struct Grid {

  coord_t bounds;
  T *items;

public:
  Grid<T>(coord_t c) : bounds{c}, arr_sz{(size_t)c.x * c.y} {
    items = new T[arr_sz];
    std::fill(items, items + arr_sz, 0);
  }

  T &at(coord_t c) {
    size_t idx = c.y * bounds.x + c.x;
    assert(idx < arr_sz);
    assert(c < bounds);

    return items[idx];
  }
  T &at(coord_t c) const {
    size_t idx = c.y * bounds.x + c.x;
    assert(idx < arr_sz);
    assert(c < bounds);

    return items[idx];
  }

private:
  size_t arr_sz;
};

Grid<char> gridify(std::istream &input);

std::ostream &operator<<(std::ostream &os, const Grid<char> g);
