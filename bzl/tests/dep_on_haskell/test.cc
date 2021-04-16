#include <iostream>

#include "HsFFI.h"
#include "tools/build_defs/haskell/tests/dep_on_haskell/Lib.stub/Lib_stub.h"
#include "tools/build_defs/haskell/tests/dep_on_haskell/Lib.stub/Sub/Lib_stub.h"

int main(int argc, char **argv) {
  hs_init(&argc, &argv);
  std::cout << inc2(1) << "\n";
  std::cout << inc3(1) << "\n";
  std::cout << mul10(123) << "\n";
  hs_exit();
  return 0;
}
