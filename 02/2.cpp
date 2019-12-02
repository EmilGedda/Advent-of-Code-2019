#include <vector>
#include <iostream>
#include <array>
#include <functional>

const std::array<std::function<int(int,int)>, 2> instructions = {
  std::plus<>(),
  std::multiplies<>()
};

int run(std::vector<int> program, int noun, int verb) {
  program[1] = noun;
  program[2] = verb;

  for(int i = 0; program[i] != 99; i += 4) {
    auto opcode = program[i] - 1;
    auto a      = program[i + 1];
    auto b      = program[i + 2];
    auto dest   = program[i + 3];
    program[dest] = instructions[opcode](program[a], program[b]);
  }

  return program[0];
}

int main() {
  std::vector<int> program;
  for(int x; std::cin >> x; std::cin.get()) {
    program.push_back(x);
  }

  for(int i = 0; i < 99; i++)
    for(int j = 0; j < 99; j++)
      if (run(program, i, j) == 19690720)
        std::cout << 100 * i + j << '\n';
}
