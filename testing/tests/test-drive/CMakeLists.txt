cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "test-drive-tests"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Playground project for testing test-drive"
)

# Get test-drive lib
set(test_lib "test-drive")
set(test_pkg "TEST_DRIVE")
set(test_url "https://github.com/fortran-lang/test-drive")

message(STATUS "Retrieving ${test_lib} from ${test_url}")
include(FetchContent)
FetchContent_Declare(
    "${test_lib}"
    GIT_REPOSITORY "${test_url}"
    GIT_TAG "HEAD"
    )
FetchContent_MakeAvailable("${test_lib}")

add_library("${test_lib}::${test_lib}" INTERFACE IMPORTED)
target_link_libraries("${test_lib}::${test_lib}" INTERFACE "${test_lib}")

# We need the module directory in the subproject before we finish the configure stage
FetchContent_GetProperties("${test_lib}" SOURCE_DIR "${test_pkg}_SOURCE_DIR")
FetchContent_GetProperties("${test_lib}" BINARY_DIR "${test_pkg}_BINARY_DIR")
if(NOT EXISTS "${${test_pkg}_BINARY_DIR}/include")
    make_directory("${${test_pkg}_BINARY_DIR}/include")
endif()

# Define src and test files
set(PROJ_SRC_DIR "${PROJECT_SOURCE_DIR}/../../../src")
set(PROJ_TEST_DIR "${PROJECT_SOURCE_DIR}")
file(GLOB PROJ_MESH_GENERATOR_SOURCES          "${PROJ_SRC_DIR}/mesh_generator/*.f90")
file(GLOB PROJ_POISSON_SOURCES          "${PROJ_SRC_DIR}/poisson/*.f90")
file(GLOB PROJ_TEST_SOURCES     "${PROJ_TEST_DIR}/tests/*.f90")

# Filter out the main.f90 files. We can only have one main() function in our tests
set(PROJ_POISSON_SOURCES_EXC_MAIN ${PROJ_POISSON_SOURCES})
set(PROJ_MESH_GENERATOR_SOURCES_EXC_MAIN ${PROJ_MESH_GENERATOR_SOURCES})
list(FILTER PROJ_POISSON_SOURCES_EXC_MAIN EXCLUDE REGEX ".*main.f90")
list(FILTER PROJ_MESH_GENERATOR_SOURCES_EXC_MAIN EXCLUDE REGEX ".*main.f90")

# Build src executables
add_executable("${PROJECT_NAME}-mesh-generator" "${PROJ_MESH_GENERATOR_SOURCES}")
add_executable("${PROJECT_NAME}-poisson" "${PROJ_POISSON_SOURCES}")

# Unit testing
set(
  tests
  "mesh_generator"
  "poisson"
)
foreach(t IN LISTS tests)
  string(MAKE_C_IDENTIFIER ${t} t) 
  list(APPEND test-srcs "${PROJ_TEST_DIR}/test_${t}.f90")
endforeach()

add_executable(
  "${PROJECT_NAME}-tester"
  "${PROJ_TEST_DIR}/main.f90"
  "${PROJ_TEST_SOURCES}"
  "${PROJ_POISSON_SOURCES_EXC_MAIN}"
  "${PROJ_MESH_GENERATOR_SOURCES_EXC_MAIN}"
)
target_link_libraries(
  "${PROJECT_NAME}-tester"
  PRIVATE
  "test-drive::test-drive"
)

foreach(t IN LISTS tests)
  add_test("${PROJECT_NAME}/${t}" "${PROJECT_NAME}-tester" "${t}")
endforeach()
