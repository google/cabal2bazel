#include <iostream.h>

#include "base/stringprintf.h"

extern "C" int foo(int x) {
  std::cout << StringPrintf("HELLO %d\n", x);
  return 5 * x;
}
