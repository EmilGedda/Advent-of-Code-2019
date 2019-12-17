
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
  std::unordered_map<int, std::unordered_map<int, int>> map;
  std::vector<int> intersections;
  char direction, wire = 0;
  std::string line;
  for(std::string line; std::getline(std::cin, line); wire++) {
    std::stringstream ss(line);
    for(int x = 0, y = 0, len = 0, distance; ss >> direction >> distance; ss.get()) {
      auto [xdiff, ydiff] = diff[direction];
      for(int i = 0; i < distance; i++) {
        len++;
        x += xdiff, y += ydiff;
        if(wire > 0 && map[x].contains(y))
          intersections.push_back(map[x][y] + len);
        if(wire == 0)
          map[x].emplace(y, len);
      }
    }
  }

  std::sort(intersections.begin(), intersections.end());
  std::cout << intersections[0] << '\n';
}
