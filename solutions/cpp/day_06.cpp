#include "iostream"
#include "vector"
#include <cstdint>
#include <string>

using namespace std;

enum class Direction : uint8_t {
  UP = 0b1,
  DOWN = 0b10,
  LEFT = 0b100,
  RIGHT = 0b1000
};

typedef vector<vector<bool>> grid_t;
typedef vector<vector<uint8_t>> dirgrid_t;

typedef struct coord {
  int x;
  int y;
} coord_t;

struct map {
  grid_t obstructions;
  struct coord location;
  Direction direction;
};

typedef uint8_t *blockmap;

struct blockmaps {
  blockmap up;
  blockmap down;
  blockmap left;
  blockmap right;
};

coord offset(Direction d) {
  switch (d) {
  case Direction::UP:
    return {0, -1};
    break;
  case Direction::DOWN:
    return {0, 1};
    break;
  case Direction::LEFT:
    return {-1, 0};
    break;
  case Direction::RIGHT:
    return {1, 0};
    break;
  }
  return {0, 0};
}

Direction next_direction(Direction d) {
  switch (d) {
  case Direction::UP:
    return Direction::RIGHT;
  case Direction::RIGHT:
    return Direction::DOWN;
  case Direction::DOWN:
    return Direction::LEFT;
  case Direction::LEFT:
    return Direction::UP;
  }
  return Direction::UP;
}

bool in_bounds(const map &map, const coord_t c) {
  bool oob = (c.x < 0 || c.y < 0) || (c.y >= map.obstructions.size()) ||
             (c.x >= map.obstructions.at(0).size());
  return !oob;
}

// Till exist or loop
void get_visited(map &map, dirgrid_t &visited) {

  while (true) {

    // Check oob
    if (!in_bounds(map, map.location))
      break;

    // Check loop
    if (visited.at(map.location.y).at(map.location.x) & (uint8_t)map.direction)
      break;

    coord_t next_coord = map.location;
    coord_t off = offset(map.direction);

    next_coord.x += off.x;
    next_coord.y += off.y;

    if (!in_bounds(map, next_coord) ||
        !map.obstructions.at(next_coord.y).at(next_coord.x)) {
      visited.at(map.location.y).at(map.location.x) |= (uint8_t)map.direction;
      map.location = next_coord;
    }

    else {
      map.direction = next_direction(map.direction);
    }
  }

  return;
}

bool loops(map &map, dirgrid_t &visited) {
  // struct map map_cpy = map;
  get_visited(map, visited);
  return (in_bounds(map, map.location));
}

void init_visited(const map &map, dirgrid_t &dest) {
  for (auto &ln : map.obstructions) {
    vector<uint8_t> ln_ret = vector<uint8_t>();
    for (bool elm : ln) {
      ln_ret.push_back(0);
    }
    dest.push_back(ln_ret);
  }
}

int solve_part_one(const map &map) {

  coord_t grid_size = {(int)map.obstructions.at(0).size(),
                       (int)map.obstructions.size()};

  struct map map_cpy = map;

  // Init visited
  dirgrid_t visited = dirgrid_t();
  init_visited(map, visited);
  get_visited(map_cpy, visited);

  int ret = 0;

  for (auto &ln : visited) {
    for (bool v : ln) {
      ret += v;
    }
  }

  return ret;
}

int solve_part_two(const map &map) {

  int ret = 0;
  int checked = 0;

  // Init visited
  struct map map_cpy = map;
  dirgrid_t visited = dirgrid_t();

  init_visited(map, visited);
  get_visited(map_cpy, visited);

  dirgrid_t empty_visited;
  init_visited(map, empty_visited);

  dirgrid_t new_visited;
  struct map new_map = map;

  int x = 0, y = 0;
  for (auto &line : visited) {
    x = 0;

    for (uint8_t cur : line) {

      if (cur) {

        new_map = map;
        new_map.obstructions.at(y).at(x) = true;
        new_visited = empty_visited;
        ret += loops(new_map, new_visited);
      }

      x++;
    }

    y++;
  }

  return ret;
}

void parse_stdin(map &map) {

  int x = 0, y = 0;
  for (std::string line; std::getline(std::cin, line);) {

    x = 0;

    vector<bool> curline = vector<bool>();
    for (char c : line) {
      curline.push_back(c == '#');

      switch (c) {
      case '#':
        break;
      case '.':
        break;
      case 'v':
        map.direction = Direction::DOWN;
        map.location = {x, y};
        break;
      case '^':
        map.direction = Direction::UP;
        map.location = {x, y};
        break;
      case '>':
        map.direction = Direction::RIGHT;
        map.location = {x, y};
        break;
      case '<':
        map.direction = Direction::LEFT;
        map.location = {x, y};
        break;
      default:
        break;
      }
      x++;
    }
    map.obstructions.push_back(curline);
    y++;
  }
}

int main(int argc, char **argv) {
  struct map map;
  parse_stdin(map);

  cout << to_string(solve_part_one(map)) << endl;

  cout << to_string(solve_part_two(map)) << endl;

    dirgrid_t visisted = dirgrid_t();
    init_visited(map, visisted);
    cout << to_string(loops(map, visisted)) << endl;
}
