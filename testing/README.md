# fortran-unit-testing
Fortran code bases often have next to zero test coverage. Any tests implemented are fully end to end and often just check that a single value in some output file is what we expect it to be. Whilst a test like this can catch a breaking change or bug, it will be unlikely to indicate where that breaking change has been introduced. The solution to this issue is unit testing. 

There are several tools/frameworks written for unit testing Fortran code. However, these are not widely adopted by research codebases. We would like to understand why that is and help to change it. 

There are several examples of good unit testing tools for other languages, such as 

- pytest (Python)
- QtTest (Qt in c++)
- J-Unit (JavaScript)

These will be used as the basis for what the recommended Fortran unit testing tool should look like. Therefore, key features from these tools shall be individually tested for each Fortran unit testing tool we select to test.

## Aims
- Test multiple fortran unit testing tools to determine which we would recommend. To do this we will
    1. [ ] Create a list of features that should be present in a "good" unit testing tool.
    2. [ ] Write tests to cover each of the key features determined in the previous step.
        - [ ] [test-drive](./tests/test-drive)
        - [ ] [pFUnit](./tests/pFUnit)
- Build template implementations for the tools we recommend.
