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
  while (!is.eof()) {
    if (is.peek() == '\n') {
      is.ignore();
      break;
    }

    ret.emplace_back();
    while (!is.eof()) {
      ret.back().emplace_back();
      is >> ret.back().back();
      if (is.get() == '\n') // \n or delim
        break;
    }
  }
  return ret;
}

template <typename T = uint64_t>
std::vector<std::pair<T, T>> parse_pairs(std::istream &is) {
  std::vector<std::pair<T, T>> ret;
  while (!is.eof()) {
    if (is.peek() == '\n') {
      is.ignore();
      break;
    }

    T a, b;
    is >> a;
    is.ignore(); // delim
    is >> b;
    ret.emplace_back(a, b);

    assert(is.peek() == '\n' || is.eof());
    is.ignore(); // \n
  }
  return ret;
}

template <typename T = uint64_t> std::vector<T> lines(std::istream &is) {
  std::vector<T> ret;
  while (!is.eof()) {
    if (is.peek() == '\n') {
      is.ignore();
      break;
    }

    ret.emplace_back();
    is >> ret.back();

    assert(is.peek() == '\n' || is.eof());
    is.ignore(); // \n
  }
  return ret;
}

template <> std::vector<std::string> lines<std::string>(std::istream &is) {
  std::vector<std::string> ret;
  for (std::string ln; getline(is, ln);) {
    if (ln.empty())
      break;
    ret.push_back(ln);
  }
  return ret;
}

template <> std::vector<uint64_t> lines<uint64_t>(std::istream &is) {
  std::vector<uint64_t> ret;
  for (std::string ln; getline(is, ln);) {
    if (ln.empty())
      break;
    ret.push_back(std::stoul(ln));
  }
  return ret;
}
