module Main where

#include "c_dep.h"

fooSize = #size foo

main = print fooSize
