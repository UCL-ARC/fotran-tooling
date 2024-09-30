# test-drive
This project offers a lightweight, procedural unit testing framework based on nothing but standard Fortran. Integration with [meson](https://mesonbuild.com/), [cmake](https://cmake.org/) and [Fortran package manager (fpm)](https://github.com/fortran-lang/fpm) is available. Alternatively, the testdrive.F90 source file can be redistributed in the project's testsuite as well.

## Features matrix

Compilers tested: gfortran (homebrew) 

| Feature | Implemented natively | Implemented manually |
|---------|----------------------|----------------------|
| Can run individual tests | Y, see [main.f90](./tests/test-drive/main.f90) | N/A |
| Mocking | No | Not implemented |
| Stubbing | No | Not implemented |
| Data driven tests | No | Yes, see verify_calculate_mesh_parameters and verify_calculate_mesh in [test_mesh_generator.f90](./tests/test_mesh_generator.f90)
| Coverage report | Y, with fpm | N/A |
| Skip tests | Y, see test_skip_example in [test_calc_pi.f90](./test-drive/tests/test_mesh_generator.f90) | N/A |

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