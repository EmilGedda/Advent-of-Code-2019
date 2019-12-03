#include <cmath>
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <utility>
#include <vector>

std::unordered_map<char, std::pair<int, int>> diff = {
  {'L', {-1, 0}},
  {'U', {0, -1}},
  {'D', {0, 1}},
  {'R', {1, 0}},
};

int main () {
  std::unordered_map<int, std::unordered_map<int, bool>> map;
  std::vector<std::pair<int, int>> intersections;
  char direction, wire = 0;
  std::string line;
  for(std::string line; std::getline(std::cin, line); wire++) {
    std::stringstream ss(line);
    for(int x = 0, y = 0, distance; ss >> direction >> distance; ss.get()) {
      auto [xdiff, ydiff] = diff[direction];
      for(int i = 0; i < distance; i++) {
        x += xdiff, y += ydiff;
        if(wire > 0 && map[x].contains(y))
          intersections.emplace_back(x, y);
        map[x][y] = true;
      }
    }
  }

  const auto manhattan = [](const auto& p){
    return std::abs(p.first) + std::abs(p.second);
  };

  const auto cmp = [&](const auto& a, const auto& b){
    return manhattan(a) < manhattan(b);
  };

  std::sort(intersections.begin(), intersections.end(), cmp);
  std::cout << manhattan(intersections[0]) << '\n';
}
