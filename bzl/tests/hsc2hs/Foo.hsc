module Main where

#include "hsc2hs_dep.h"

fooSize = #size foo

#ifndef __GLASGOW_HASKELL__
#error "The macro __GLASGOW_HASKELL__ isn't defined"
#endif

main = print fooSize
