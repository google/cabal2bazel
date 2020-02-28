#include <iostream>

#include "HsFFI.h"
#include "tools/build_defs/haskell/tests/dep_on_haskell/CDep.stub/CDep_stub.h"

int main(int argc, char **argv) {
  hs_init(&argc, &argv);
  int result = plus43(1);
  hs_exit();
  if (result != 44) {
    std::cerr << "Incorrect result; expected 43, got: " << result << "\n";
    return 1;
  }
  return 0;
}
