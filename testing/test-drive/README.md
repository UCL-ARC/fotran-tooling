# test-drive
This project offers a lightweight, procedural unit testing framework based on nothing but standard Fortran. Integration with [meson](https://mesonbuild.com/), [cmake](https://cmake.org/) and [Fortran package manager (fpm)](https://github.com/fortran-lang/fpm) is available. Alternatively, the testdrive.F90 source file can be redistributed in the project's testsuite as well.

## Running the tests

The test-drive tests will run with the rest of the [tests](../README.md#running-the-tests) in the repo.

There is a known issue that the tests for test-drive itself will also be ran by `ctest` as shown below
```sh
$ ctest   
Test project /Users/connoraird/work/fortran-tooling/build
    Start 1: fortran-tooling-test-drive/mesh_generator
1/4 Test #1: fortran-tooling-test-drive/mesh_generator ...   Passed    0.33 sec
    Start 2: test-drive/all-tests
2/4 Test #2: test-drive/all-tests ........................   Passed    0.33 sec
    Start 3: test-drive/check
3/4 Test #3: test-drive/check ............................   Passed    0.01 sec
    Start 4: test-drive/select
4/4 Test #4: test-drive/select ...........................   Passed    0.01 sec

100% tests passed, 0 tests failed out of 4

Total Test time (real) =   0.69 sec
```


## Features matrix

Compilers tested: gfortran (homebrew) 

| Feature | Implemented natively | Implemented manually |
|---------|----------------------|----------------------|
| Can run individual tests | No | Yes, see [main.f90](./main.f90). However, this requires running the test executable directly without ctest. |
| Mocking | No | Not implemented |
| Stubbing | No | Not implemented |
| Data driven tests | No | Yes, see verify_calculate_mesh_parameters and verify_calculate_mesh in [test_mesh_generator.f90](./test_mesh_generator.f90)
| Coverage report | Yes, with fpm | N/A |
| Skip tests | Yes, see test_skip_example in [test_calc_pi.f90](./test_mesh_generator.f90) | N/A |

## Pros
- Lightweight, procedural unit testing framework based on nothing but standard Fortran.
- Since test-drive is written is standard fortran, any customisation we implement could be carried over to the open-source test-drive code.

## Cons 
- Slow at releasing. Last release was in 2021 (3 years ago).
- Most standard test festures such as test paramaterisation and mocking are not provided by the test-drive library.
- A fair amount of boiler plate code is required to get things up and running.

## Building
- `fpm` provided a very convenient way to [get started](https://fpm.fortran-lang.org/tutorial/dependencies.html#adding-a-testing-framework) with `test-drive`. However, it is unlikely that a long running Fortran project written in Fortran 95, for example, will be using `fpm`. It is more likely that `cmake` is in use.  
- `cmake` can be used to install

## Resources
- Repository: https://github.com/fortran-lang/test-drive