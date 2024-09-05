# pFUnit
pFUnit is a unit testing framework enabling JUnit-like testing of serial and MPI-parallel software written in Fortran. Limited support for OpenMP is also provided in the form of managing exceptions in a thread-safe manner.

pFUnit uses several advanced Fortran features, especially object oriented capabilities, to offer a convenient, lightweight mechanism for Fortran developers to create and run software tests that specify the desired behavior for a given piece of code.

This framework was originally created by developers from NASA and NGC TASC.

## Pros

## Cons
- Due to the heavy use of F2003 object oriented features and a smattering of F2008 features, only relatively recent Fortran compilers are able to correctly build pFUnit. 

## Resources
- Repository: https://github.com/Goddard-Fortran-Ecosystem/pFUnit
- Webinar: [Testing Fortran Software with pFUnit](https://ideas-productivity.org/events/hpcbp-028-pfunit)