#!/bin/bash

set -eu

sed s/TWO/two/ < $2 > $3
