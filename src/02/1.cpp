#include <vector>
#include <iostream>
#include <array>
#include <functional>

int main() {
  std::vector<int> program;
  for(int x; std::cin >> x; std::cin.get())
    program.push_back(x);

  program[1] = 12;
  program[2] = 2;

  std::array<std::function<int(int,int)>, 2> instructions = {
    std::plus<>(),
    std::multiplies<>()
  };

  for(int i = 0; program[i] != 99; i += 4) {
    auto opcode = program[i] - 1;
    auto a      = program[i + 1];
    auto b      = program[i + 2];
    auto dest   = program[i + 3];
    program[dest] = instructions[opcode](program[a], program[b]);
  }

  std::cout << program[0] << '\n';
}
