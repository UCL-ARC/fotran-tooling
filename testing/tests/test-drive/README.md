# test-drive
This project offers a lightweight, procedural unit testing framework based on nothing but standard Fortran. Integration with [meson](https://mesonbuild.com/), [cmake](https://cmake.org/) and [Fortran package manager (fpm)](https://github.com/fortran-lang/fpm) is available. Alternatively, the testdrive.F90 source file can be redistributed in the project's testsuite as well.

## Pros
- Lightweight, procedural unit testing framework based on nothing but standard Fortran

## Cons 
- Slow at releasing. Last release was in 2021 (3 years ago).

## Building
- `fpm` provided a very convenient way to [get started](https://fpm.fortran-lang.org/tutorial/dependencies.html#adding-a-testing-framework) with `test-drive`. However, it is unlikely that a long running Fortran project written in Fortran 95, for example, will be using `fpm`. It is more likely that `cmake` is in use.  
- `cmake` can be used to install

## Resources
- Repository: https://github.com/fortran-lang/test-drive