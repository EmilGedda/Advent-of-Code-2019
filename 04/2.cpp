#include <algorithm>
#include <array>
#include <iostream>

bool adjacency(const std::array<int, 6>& num, const std::array<int, 10>& bucket) {
  for(int i = 0; i < 5; i++)
    if(num[i] == num[i + 1] && bucket[num[i]] == 2)
      return true;
  return false;
}

int main() {
  // 236491-713787
  const int start = 236666;
  const int stop  = 699999;
  std::array<int, 6> digits;
  int count = 0;
  for(int num = start; num <= stop; num++) {
    std::array<int, 10> bucket{};
    int n = num, i = 5;
    while (n) {
      int digit = n % 10;
      n /= 10;
      digits[i--] = digit;
      bucket[digit]++;
    }
    count += std::is_sorted(digits.begin(), digits.end()) && adjacency(digits, bucket);
  }
  std::cout << count << '\n';
}
