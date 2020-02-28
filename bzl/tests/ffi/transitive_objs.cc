// Test an implicit dependency on libsdtc++
#include <iostream>

#include "tools/build_defs/haskell/tests/ffi/three.h"

extern "C" int triple(int x) {
  std::cout << "Hello\n";
  return three() * x;
}
