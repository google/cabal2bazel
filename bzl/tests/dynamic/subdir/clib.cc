#include <iostream>

extern "C" {
int triple(int x) {
  std::cout << "Input=" << x << "\n";
  return 3 * x;
}
}
