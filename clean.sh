#!/bin/bash

if ls src/*.o 1> /dev/null 2>&1; then
  rm src/*.o
fi

if ls src/*.so 1> /dev/null 2>&1; then
  rm src/*.so
fi
