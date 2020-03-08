#include <iostream.h>

#include "third_party/absl/strings/str_format.h"

extern "C" int foo(int x) {
  std::cout << absl::StrFormat("HELLO %d\n", x);
  return 5 * x;
}
