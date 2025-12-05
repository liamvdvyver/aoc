#include <cassert>
#include <iostream>
#include <vector>

// May be invalidated if string goes out of scope
constexpr std::vector<std::string_view> split(std::string_view str,
                                              char delim = ' ') {
  std::vector<std::string_view> ret;
  size_t next_pos = -1;
  while ((next_pos = str.find(delim)) != str.npos) {
    ret.push_back(str.substr(next_pos));
    str = str.substr(0, next_pos + 1);
  }
  return ret;
}

template <typename T = uint64_t, bool Delimited = true>
std::vector<std::vector<T>> parse_delim(std::istream &is) {
  std::vector<std::vector<T>> ret;
  ret.emplace_back();
  while (true) {
    if (is.eof()) {
      break;
    }
    if (is.peek() == '\n' && ret.empty()) {
      is.ignore();
      break;
    }
    if (is.peek() == '\n' && (ret.back().empty() || is.eof())) {
      is.ignore();
      ret.pop_back();
      break;
    }

    if (is.peek() == '\n') {
      is.ignore();
      ret.emplace_back();
    } else {
      ret.back().emplace_back();
      is >> ret.back().back();
      if (Delimited && is.peek() != '\n') {
        is.ignore(); // chuck away delimiter
      }
    }
  }
  return ret;
}

template <typename T = uint64_t> std::vector<T> lines(std::istream &is) {
  std::vector<T> ret;
  while (true) {
    if (is.peek() == '\n' || is.eof()) {
      is.ignore();
      break;
    }

    ret.emplace_back();
    is >> ret.back();
    if (is.eof()) {
      break;
    }
    assert(is.get() == '\n');
  }
  return ret;
}
