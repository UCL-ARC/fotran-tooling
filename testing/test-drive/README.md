# test-drive
This project offers a lightweight, procedural unit testing framework based on nothing but standard Fortran. Integration with [meson](https://mesonbuild.com/), [cmake](https://cmake.org/) and [Fortran package manager (fpm)](https://github.com/fortran-lang/fpm) is available. Alternatively, the testdrive.F90 source file can be redistributed in the project's testsuite as well.

## Running the tests

All of the tests written with test-drive can be ran by following [#running all of the tests](../README.md#running-all-of-the-tests). However, we can also run a single testsuite or an individual test directly using the test executable. For example to run the `mesh_generator` tests we can run
```sh
$ <root of repo>/build/testing/test-drive/test_fortran-tooling-test-drive mesh_generator
```
and to run the single `mesh_generator` test `test_calculate_mesh_parameters_5_1`
```sh
$ <root of repo>/build/testing/test-drive/test_fortran-tooling-test-drive mesh_generator test_calculate_mesh_parameters_5_1
```

## Features matrix

Compilers tested: gfortran (homebrew) 

| Feature | Implemented natively | Implemented manually |
|---------|----------------------|----------------------|
| Can run individual tests | No | Yes, see [main.f90](./main.f90). However, this requires running the test executable directly without ctest. |
| Mocking | No | Not implemented |
| Stubbing | No | Not implemented |
| Data driven tests | No | Yes, but this is very cumbersome. See `verify_calculate_mesh_parameters` and `verify_calculate_mesh` in [test_mesh_generator.f90](./test_mesh_generator.f90)
| Coverage report | Yes, with fpm | N/A |
| Skip tests | Yes, see `test_skip_example` in [test_mesh_generator.f90](./test_mesh_generator.f90) | N/A |

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