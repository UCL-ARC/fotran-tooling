#!/bin/bash

# set default input options
clean_build=false
build_executables=false
run_tests=false
run_main=false
cmake_dir="/Users/connoraird/work/fortran-unit-testing/tests/test-drive"

help() {
  echo "Usage:"
  echo "    -h     Display this help message."
  echo "    -c     Clean, configure and generate (deletes Build directory and calls cmake)."
  echo "    -b     Builds the executables via make."
  echo "    -t     Run all tests."
  echo "    -r     Run the main program."
  exit 0
}

# check for no input arguments and show help
if [ $# -eq 0 ];
then
    ./build.sh -h
    exit 0
fi

# parse input arguments
while getopts "hcbtr" opt
do
  case ${opt} in
    h  ) help;;
    c  ) clean_build=true;;
    b  ) build_executables=true;;
    t  ) run_tests=true;;
    r  ) run_main=true;;
    \? ) echo "Invalid option: $OPTARG" >&2; exit 1;;
  esac
done
shift $((OPTIND -1))


if [ "$clean_build" = "true" ]
then
    rm -rf build
    mkdir build
    cd build
    cmake "${cmake_dir}"
    EXITCODES+=$?
else
    mkdir -p build
    cd build
fi

if [ "$build_executables" = "true" ]
then
    make
fi

if [ "$run_tests" = "true" ]
then
    ./fortran-unit-testing-with-test-drive-tester
fi

if [ "$run_main" = "true" ]
then
    ./fortran-unit-testing-with-test-drive
fi
