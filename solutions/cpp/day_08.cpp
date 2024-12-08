#include "iostream"

#include <grid.h>
#include <map>
#include <numeric>
#include <vector>

using namespace std;

typedef std::map<char, std::vector<coord_t>> freq_map;

freq_map fill_freqs(const Grid<char> &in) {

  freq_map ret{};
  for (coord_t c : row_major(in.bounds)) {
    if (in.at(c) != '.') {
      ret[in.at(c)].push_back(c);
    }
  }
  return ret;
}

vector<coord_t> part_one_antis(coord_t a, coord_t b, coord_t ub) {
  return {a - (b - a), b - (a - b)};
}

vector<coord_t> part_two_antis(coord_t a, coord_t b, coord_t ub) {
  vector<coord_t> ret;

  coord_t offset = a - b;
  int factor = gcd(offset.x, offset.y);
  offset = offset / factor;

  coord_t origin = {0, 0};
  for (coord_t c = a; c >= origin && c < ub; c = c + offset) {
    ret.push_back({c});
  }

  for (coord_t c = a - offset; c >= origin && c < ub; c = c - offset) {
    ret.push_back({c});
  }

  return ret;
}

int solve_part(const Grid<char> &in,
               vector<coord_t> (*anti_func)(coord_t, coord_t, coord_t)) {

  int ret = 0;

  const Grid<bool> antis = Grid<bool>(in.bounds);
  const freq_map freqs = fill_freqs(in);

  for (auto &entry : freqs) {

    vector<coord_t> vals = entry.second;

    for (int i = 0; i < vals.size() - 1; i++) {
      for (int j = i + 1; j < vals.size(); j++) {

        coord_t ai = vals.at(i);
        coord_t aj = vals.at(j);

        for (coord_t c : anti_func(ai, aj, in.bounds)) {

          if (c >= coord_t{0, 0} && c < in.bounds) {
            antis.at(c) = true;
          }
        }
      }
    }
  }

  for (coord_t c : row_major(antis.bounds)) {

    ret += antis.at(c);
  }

  return ret;
}

int main(int argc, char **argv) {
  Grid<char> in = stdin_grid();
  cout << to_string(solve_part(in, &part_one_antis)) << endl;
  cout << to_string(solve_part(in, &part_two_antis)) << endl;
}
