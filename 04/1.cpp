#include <algorithm>
#include <array>
#include <iostream>

bool adjacency(const std::array<int, 6>& num) {
  for(int i = 0; i < 5; i++)
    if(num[i] == num[i + 1])
      return true;
  return false;
}

int main() {
  // 236491-713787
  const int start = 236666;
  const int stop =  700000;
  int count = 0;
  for(int num = start; num <= stop; num++) {
    std::array<int, 6> digits;
    int n = num, i = 5;
    while (n) {
      digits[i--] = n % 10;
      n /= 10;
    }
    count += std::is_sorted(digits.begin(), digits.end()) && adjacency(digits);
  }
  std::cout << count << '\n';
}
